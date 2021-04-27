// See LICENSE for license details.

package chiselucl
package backend

import java.io.Writer

import chiselucl.transforms._
import chiselucl.annotations._

import firrtl._
import firrtl.analyses._
import firrtl.ir._
import firrtl.passes._
import firrtl.transforms._
import firrtl.traversals.Foreachers._
import firrtl.PrimOps._
import firrtl.Utils._
import firrtl.stage.Forms
import firrtl.options.Dependency
import firrtl.annotations.ReferenceTarget

import MemPortUtils.{memPortField}

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashSet}

class WriterState(writer: Writer = new java.io.StringWriter, debugOutput: Boolean = false) {
  def write(s: String): Unit = writer.write(s)
  def serialize(): String = writer.toString()

  def debugComment(s: String): Unit = {
    if (debugOutput) {
      write(" " * indentLevel)
      write(s"// ${s}\n")
    }
  }

  private var indentValue: Int = 0
  def increaseIndent() = indentValue += 2
  def decreaseIndent() = indentValue -= 2
  def indentLevel = indentValue
  def indentString = " " * indentValue
  def zeroIndent() = {
    indentValue = 0
  }
}

class UclidEmitter extends Transform with DependencyAPIMigration {

  override def prerequisites = Forms.LowForm ++
    Seq(
      Dependency[ReplaceTruncatingArithmetic],
      Dependency[DeadCodeElimination],
      Dependency[SimplifyRegUpdate],
      Dependency[RemoveTail],
      Dependency(InferStrictBooleans)
    )

  override def optionalPrerequisites = Nil
  override def optionalPrerequisiteOf = Nil
  override def invalidates(a: Transform): Boolean = false

  val outputSuffix = ".ucl"

  private def equalize_binary_op(p: Pair[Type, Type], arg0: String, arg1: String) : Pair[String, String] = {
    val extra_bits = get_width(p._1) - get_width(p._2)
      //Not throwing error if arg0 is shorter than shift amount
    if (extra_bits == 0) {
      (arg0, arg1)
    } else if (extra_bits < 0) {
      val adj_arg = p._1 match {
        case UIntType(_) =>
          s"bv_zero_extend(${-1 * extra_bits}, ${arg0})"
        case SIntType(_) =>
          s"bv_sign_extend(${-1 * extra_bits}, ${arg0})"
      }
      (adj_arg, arg1)
    } else {
      val adj_arg = p._2 match {
        case UIntType(_) =>
          s"bv_zero_extend(${extra_bits}, ${arg1})"
        case SIntType(_) =>
          s"bv_sign_extend(${extra_bits}, ${arg1})"
      }
      (arg0, adj_arg)
    }
  }

  private def memAddrType(mem: DefMemory): UIntType = UIntType(IntWidth(chisel3.util.log2Ceil(mem.depth)))

  private def serialize_rhs_ref(wr: WRef, rhsPrimes: Boolean)(implicit wState: WriterState): String = {
    if (rhsPrimes) s"${wr.name}'" else s"${wr.name}"
  }

  private def serialize_unop(p: DoPrim, arg0: String): String = p.op match {
    case Neg => s"-$arg0"
    case Not => s"~$arg0"
    // TODO: Handle asUInt operator
    case AsUInt => arg0
    // TODO: Handle asSInt operator
    case AsSInt => arg0
    case Cvt =>
      // Converts unsigned to signed
      val dstLen = get_width(p.tpe)
      val srcLen = get_width(p.args.head.tpe)
      val diff = dstLen - srcLen
      if (diff == 0) {
        arg0
      } else {
        s"bv_sign_extend($diff, $arg0)"
      }
    case Andr | Orr | Xorr =>
      // Simulate bitwise reduction operators
      (0 until get_width(p.args.head.tpe)).map(i => s"$arg0[$i:$i]").mkString(
        p.op match {
          case Andr => " & "
          case Orr => " | "
          case Xorr => " ^ "
        }
      )
    case _ => throwInternalError(s"Illegal unary operator: ${p.op}")
  }

  private def serialize_shamt_exp(p: DoPrim, shamtArg: String): String = p.op match {
    case Dshlw | Dshr =>
      val extra_bits = get_width(p.args(0).tpe) - get_width(p.args(1).tpe)
      if (extra_bits < 0) {
        throwInternalError(s"Shift amount must be wider than shifted value")
      } else if (extra_bits == 0) {
        shamtArg
      } else {
        s"bv_zero_extend(${extra_bits}, ${shamtArg})"
      }
    //Special case (might be temporary)
    case Dshl =>
      val extra_bits = get_width(p.args(0).tpe) - get_width(p.args(1).tpe)
      //Not throwing error if arg0 is shorter than shift amount
      if (extra_bits <= 0) {
        shamtArg
      } else {
        s"bv_zero_extend(${extra_bits}, ${shamtArg})"
      }

    case Shl | Shr => shamtArg
    case _ => throwInternalError(s"Illegal shift operator: ${p.op}")
  }

  private def serialize_binop(p: DoPrim, arg0: String, arg1: String): String = p.op match {
    case Add =>
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      p.tpe match {
        case UIntType(_) => s"bv_zero_extend(1, ${args._1}) + bv_zero_extend(1, ${args._2})"
        case SIntType(_) => s"bv_sign_extend(1, ${args._1}) + bv_sign_extend(1, ${args._2})"
      }
    case Addw => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"${args._1} + ${args._2}"
    }
    case Sub =>
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      //TODO: Check that this sign extension is correct, if they are different types, then shouldn't we choose to sign extend and zero extend selectively
      p.tpe match {
        case UIntType(_) => s"bv_zero_extend(1, ${args._1}) - bv_zero_extend(1, ${args._2})"
        case SIntType(_) => s"bv_sign_extend(1, ${args._1}) - bv_sign_extend(1, ${args._2})"
      }
    case Subw =>
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"${args._1} - ${args._2}"
    case Lt => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} < ${args._2}) then (1bv1) else (0bv1)"
    }
    case Leq => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} <= ${args._2}) then (1bv1) else (0bv1)"
    }
    case Gt => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} > ${args._2}) then (1bv1) else (0bv1)"
    }
    case Geq => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} >= ${args._2}) then (1bv1) else (0bv1)"
    }
    case Eq => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} == ${args._2}) then (1bv1) else (0bv1)"
    }
    case Neq => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"if (${args._1} != ${args._2}) then (1bv1) else (0bv1)"
    }
    case And => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"${args._1} & ${args._2}"
    }
    case Or => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"${args._1} | ${args._2}"
    }
    case Xor => {
      val args = equalize_binary_op((p.args(0).tpe, p.args(1).tpe), arg0, arg1)
      s"${args._1} ^ ${args._2}"
    }
    case Bits => s"${arg0}[${arg1}]"
    case Shl | Dshlw =>
      //TODO: Uclid shifts may not work as intended
      //TODO: Is this the same as concatenating n zeros at the end
      val shamt = serialize_shamt_exp(p, arg1)
      val extra_bits = get_width(p.tpe) - get_width(p.args(0).tpe)
      val adj_arg = extra_bits match {
        case 0 => arg0
        case _ => p.args(0).tpe match {
          case UIntType(_) => s"bv_zero_extend($extra_bits, ${arg0})"
          case SIntType(_) => s"bv_sign_extend($extra_bits, ${arg0})"
        }
      }
      shamt match {
        case "0" => adj_arg
        case _ => s"bv_left_shift(${shamt}, ${adj_arg})"
      }
    case Dshl =>
      //TODO: Resolve if dshl relates to hardware synthesis
      val shamt = serialize_shamt_exp(p, arg1)
      val extra_bits = get_width(p.tpe) - get_width(p.args(0).tpe)
      val adj_arg = extra_bits match {
        case 0 => arg0
        case _ => p.args(0).tpe match {
          case UIntType(_) => s"bv_zero_extend($extra_bits, ${arg0})"
          case SIntType(_) => s"bv_sign_extend($extra_bits, ${arg0})"
        }
      }
      shamt match {
        case "0" => adj_arg
        case _ => s"bv_left_shift(${shamt}, ${adj_arg})"
      }
    case Dshr =>
      val shamt = serialize_shamt_exp(p, arg1)
      p.tpe match {
        case UIntType(_) => s"bv_l_right_shift(${shamt}, ${arg0})"
        case SIntType(_) => s"bv_a_right_shift(${shamt}, ${arg0})"
      }
    case Shr =>
      val shamt = serialize_shamt_exp(p, arg1)
      p.tpe match {
        case UIntType(_) => {
          s"${arg0}[${get_width(p.args(0).tpe) - 1}:${shamt}]"
        }
        case SIntType(_) => s"bv_a_right_shift(${shamt}, ${arg0})"
      }
    case Cat => s"${arg0} ++ ${arg1}"
    case Pad => {
      val extra_bits = p.consts(0) - get_width(p.args(0).tpe)
      p.tpe match {
        case UIntType(_) if (extra_bits > 0) => s"bv_zero_extend(${extra_bits}, ${arg0})"
        case SIntType(_) if (extra_bits > 0) => s"bv_sign_extend(${extra_bits}, ${arg0})"
        case _ => s"${arg0}"
      }
    }
    case Mul => {
      //TODO: Deal with signed and unsigned
      val adj_arg0 = p.args(0).tpe match {
        case UIntType(_) => s"bv_zero_extend(${get_width(p.tpe) - get_width(p.args(0).tpe)}, ${arg0})"
        case SIntType(_) => s"bv_sign_extend(${get_width(p.tpe) - get_width(p.args(0).tpe)}, ${arg0})"
      }
      val adj_arg1 = p.args(1).tpe match {
        case UIntType(_) => s"bv_zero_extend(${get_width(p.tpe) - get_width(p.args(1).tpe)}, ${arg1})"
        case SIntType(_) => s"bv_sign_extend(${get_width(p.tpe) - get_width(p.args(1).tpe)}, ${arg1})"
      }
      s"$adj_arg0 * $adj_arg1"
    }
    case _ => throwInternalError(s"Illegal binary operator: ${p.op}")
  }

  private def serialize_ternop(p: DoPrim, arg0: String, arg1: String, arg2: String): String = p.op match {
    case Bits => s"${arg0}[${arg1}:${arg2}]"
    case _ => throwInternalError(s"Illegal ternary operator: ${p.op}")
  }

  private def serialize_prim(p: DoPrim, rhsPrimes: Boolean)(implicit wState: WriterState): String = (p.args.length, p.consts.length) match {
    case (2, 0) => serialize_binop(p, serialize_rhs_exp(p.args(0), rhsPrimes), serialize_rhs_exp(p.args(1), rhsPrimes))
    case (1, 0) => serialize_unop(p, serialize_rhs_exp(p.args(0), rhsPrimes))
    case (1, 2) => serialize_ternop(p, serialize_rhs_exp(p.args(0), rhsPrimes), p.consts(0).toString, p.consts(1).toString)
    case (1, 1) => serialize_binop(p, serialize_rhs_exp(p.args(0), rhsPrimes), p.consts(0).toString)
    case (0, 2) => serialize_binop(p, p.consts(0).toString, p.consts(1).toString)
    case (0, 1) => serialize_unop(p, p.consts(0).toString)
    case _ => throwInternalError(s"Illegal primitive operator operands")
  }

  private def serialize_mux(m: Mux, rhsPrimes: Boolean)(implicit wState: WriterState): String = {
    val i = serialize_rhs_exp(m.cond, rhsPrimes)
    val t = serialize_rhs_exp(m.tval, rhsPrimes)
    val e = serialize_rhs_exp(m.fval, rhsPrimes)
    val rets = equalize_binary_op((m.tval.tpe, m.fval.tpe), t, e)
    //TODO: Check this fix for muxes since we don't assume uint<1> to be booleans

    s"if (($i) == 1bv1) then (${rets._1}) else (${rets._2})"
  }

  private def get_width(w: Width): Int = w match {
    case IntWidth(iw: BigInt) => iw.intValue
    case _ => throwInternalError(s"Types must have integral widths")
  }

  private def get_width(tpe: Type): Int = tpe match {
    case UIntType(w: Width) => get_width(w)
    case SIntType(w: Width) => get_width(w)
    case _ => throwInternalError(s"Cannot get width of type ${tpe}")
  }

  private def serialize_rhs_exp(e: Expression, rhsPrimes: Boolean)(implicit wState: WriterState): String = e match {
    case wr: WRef => serialize_rhs_ref(wr, rhsPrimes)
    case ws: WSubField => serialize_rhs_ref(WRef(LowerTypes.loweredName(ws)), rhsPrimes)
    case m: Mux => serialize_mux(m, rhsPrimes)
    case p: DoPrim => serialize_prim(p, rhsPrimes)
    case ul: UIntLiteral => s"${ul.value}bv${get_width(ul.width)}"
    case sl: SIntLiteral => s"${sl.value}bv${get_width(sl.width)}"
    case ToStrictBoolean(e) => s"(${serialize_rhs_exp(e, rhsPrimes)}) == 1bv1"
    case _ =>
      println(s"Unsupported rhs expression: $e")
      throwInternalError(s"Trying to emit unsupported expression")
  }

  private def serialize_lhs_exp(e: Expression): String = e match {
    case wr: WRef => wr.name
    case sub: WSubField => LowerTypes.loweredName(sub)
    case _ =>
      println(s"Unsupported lhs expression: $e")
      throwInternalError(s"Trying to emit unsupported expression")
  }

  private def serialize_type(tpe: Type): String = tpe match {
    case UIntType(w: Width) => s"bv${get_width(w)}"
    case SIntType(w: Width) => s"bv${get_width(w)}"
    case StrictBooleanType => "boolean"
    case t =>
      throwInternalError(s"Trying to emit unsupported type: ${t.serialize}")
  }

  private def indent_line()(implicit wState: WriterState): Unit = {
    wState write (" " * wState.indentLevel)
  }

  private def emit_port(p: Port)(implicit wState: WriterState): Unit = {
    indent_line()
    val dir = if (p.direction == Input) "input" else "output"
    val uclType = serialize_type(p.tpe)
    wState write s"${dir} ${p.name} : ${uclType};\n"
  }

  private def emit_reg_decl(r: DefRegister)(implicit wState: WriterState): Unit = {
    indent_line()
    val uclType = serialize_type(r.tpe)
    wState write s"var ${r.name} : ${uclType};\n"
  }

  private def emit_const_decl(name: String, tpe: Type)(implicit wState: WriterState): Unit = {
    indent_line()
    val uclType = serialize_type(tpe)
    wState write s"const ${name} : ${uclType};\n"
  }

  private def emit_mem_decl(m: DefMemory)(implicit wState: WriterState): Unit = {
    indent_line()
    val uclType = serialize_type(m.dataType)
    val addrType = serialize_type(memAddrType(m))
    wState write s"var ${m.name} : [$addrType]${uclType};\n"
  }

  private def emit_node_decl(r: DefNode)(implicit wState: WriterState): Unit = {
    indent_line()
    val uclType = serialize_type(r.value.tpe)
    wState write s"var ${r.name} : ${uclType};\n"
  }

  private def emit_wire_decl(wire: DefWire)(implicit wState: WriterState): Unit = {
    indent_line()
    val uclType = serialize_type(wire.tpe)
    wState write s"var ${wire.name} : ${uclType};\n"
  }

  private def emit_init(mems: Seq[DefMemory], nodes: Seq[DefNode], comb_assigns: Seq[Connect], init_assumptions: Seq[(String, Boolean)])
    (implicit wState: WriterState): Unit = {
    indent_line()
    wState.write(s"init {\n")
    wState.increaseIndent()
    for ((k, v) <- init_assumptions) {
      indent_line()
      val asNum = if (v) 1 else 0
      wState.write(s"assume ${k} == ${asNum}bv1;\n")
    }
    // TODO: these may need toposort
    nodes.foreach(emit_node_init(_))
    //TODO: Comb assigns need to be adjusted for width
    comb_assigns.foreach(emit_wire_init(_))
    wState.decreaseIndent()
    indent_line()
    wState.write("}\n")
  }

  private def emit_node_init(n: DefNode)(implicit wState: WriterState): Unit = {
    indent_line()
    wState write s"${n.name} = "
    wState write serialize_rhs_exp(n.value, rhsPrimes = false)
    wState write ";\n"
  }

  //NOTE: This is only used to process combinational assigns (which are certain connect statements in the init block
  private def emit_wire_init(c: Connect)(implicit wState: WriterState): Unit = {
    require(get_width(c.loc.tpe) >= get_width(c.expr.tpe))
    val lhs = serialize_lhs_exp(c.loc)
    val rhs = serialize_rhs_exp(c.expr, rhsPrimes = false)
    val args = equalize_binary_op((c.loc.tpe, c.expr.tpe), lhs, rhs)
    indent_line()
    wState write s"${args._1} = ${args._2}"
    wState write ";\n"
  }

  private def emit_inst_decl(i: WDefInstance, f : LinkedHashMap[String, Expression], m : Module)(implicit wState: WriterState): Unit = {
    val inst_params = new ArrayBuffer[String]()
    for (p <- m.ports) {
      f.get(p.name) match {
        case Some(v) => {
          //TODO: Fix this connection assumption by implementing partial connects
          require(get_width(p.tpe) >= get_width(v.tpe))
          val args = equalize_binary_op((p.tpe, v.tpe), p.name, serialize_rhs_exp(v, true))
          inst_params.append(s"${args._1} : (${args._2})")
        }
        case None => if (p.tpe != ClockType) {
          indent_line();
          wState write s"var ${i.name}_${p.name} : ${serialize_type(p.tpe)};\n"
          if (p.direction == firrtl.ir.Input) {
            inst_params.append(s"${p.name} : (${i.name}_${p.name}')") // primed
          } else {
            inst_params.append(s"${p.name} : (${i.name}_${p.name})")
          }
        }
      }
    }

    indent_line()
    wState write s"instance ${i.name} : ${i.module}(${inst_params.mkString(", ")});\n"
  }

  private def emit_inst_next(i: WDefInstance)(implicit wState: WriterState): Unit = {
    indent_line()
    wState write s"next(${i.name});\n"
  }

  private def emit_node_assignment(n: DefNode)(implicit wState: WriterState): Unit = {
    indent_line()
    wState write s"${n.name}' = "
    wState write serialize_rhs_exp(n.value, rhsPrimes = true)
    wState write ";\n"
  }

  private def emit_connect(c: Connect, rhsPrimes: Boolean)(implicit wState: WriterState): Unit = {
    require(get_width(c.loc.tpe) >= get_width(c.expr.tpe))
    val lhs = serialize_lhs_exp(c.loc)
    val rhs = serialize_rhs_exp(c.expr, rhsPrimes)
    val args = equalize_binary_op((c.loc.tpe, c.expr.tpe), lhs, rhs)
    indent_line()
    wState write s"${args._1}' = ${args._2}"
    wState write ";\n"
  }

  private def emit_mem_reads(m: DefMemory)(implicit wState: WriterState): Unit = {
    for (r <- m.readers) {
      val lhs = serialize_lhs_exp(memPortField(m, r, "data"))
      val array = serialize_rhs_exp(WRef(m.name), true)
      val memEn = serialize_rhs_exp(memPortField(m, r, "en"), true)
      val memIdx = serialize_rhs_exp(memPortField(m, r, "addr"), rhsPrimes = true)
      indent_line()
      wState write s"if (${memEn} == 1bv1) { ${lhs}' = $array[$memIdx]; } else { havoc ${lhs}; }\n"
    }
  }


  private def writeProcedureName(m: DefMemory): String = s"write_mem_${m.name}"

  private case class WritePort(name: String, addr: String, data: String, en: String, mask: String)
  private def emit_mem_write_procedure(m: DefMemory)(implicit wState: WriterState): Unit = {
    indent_line()
    val pname = writeProcedureName(m)
    wState.write(s"procedure $pname() modifies ${m.name}, havoc_${m.name};\n")
    indent_line()
    wState.write("{\n")
    val ports = m.writers.map { wr =>
      val en = serialize_lhs_exp(memPortField(m, wr, "en"))
      val mask = serialize_lhs_exp(memPortField(m, wr, "mask"))
      val addr = serialize_lhs_exp(memPortField(m, wr, "addr"))
      val data = serialize_lhs_exp(memPortField(m, wr, "data"))
      WritePort(wr, addr, data, en, mask)
    }
    wState.increaseIndent()
    for (p <- ports) {
      indent_line()
      //TODO: Check that changing this to a bitwise and is correct
      wState.write(s"if ((${p.en} & ${p.mask}) == 1bv1) {\n")
      wState.increaseIndent()
      indent_line()
      wState.write(s"${m.name}[${p.addr}] = ${p.data};\n")
      wState.decreaseIndent()
      indent_line()
      wState.write("}\n")
    }
    // Check for address collisions
    for (Seq(p1, p2) <- ports.combinations(2)) {
      indent_line()
      wState.write(s"if (${p1.en} && ${p2.en} && ${p1.mask} && ${p2.mask} && (${p1.addr} == ${p2.addr})) {\n")
      wState.increaseIndent()
      indent_line()
      wState.write(s"havoc havoc_${m.name};\n")
      indent_line()
      wState.write(s"${m.name}[${p1.addr}] = havoc_${m.name};\n")
      wState.decreaseIndent()
      indent_line()
      wState.write("}\n")
    }
    wState.decreaseIndent()
    indent_line()
    wState.write("}\n");
  }

  private def emit_mem_writes(m: DefMemory)(implicit wState: WriterState): Unit = {
    indent_line()
    val pname = writeProcedureName(m)
    wState.write(s"call $pname();\n")
  }

  private def emit_open_module_scope(m: Module)(implicit wState: WriterState): Unit = {
    wState write s"module ${m.name} {\n"
    wState.increaseIndent()
  }

  private def emit_open_next_scope()(implicit wState: WriterState): Unit = {
    indent_line()
    wState write s"next {\n"
    wState.increaseIndent()
  }

  private def emit_close_scope()(implicit wState: WriterState): Unit = {
    wState.decreaseIndent()
    indent_line()
    wState write s"}\n"
  }

  private def emit_module_level_annos(cs: CircuitState, m: Module)(implicit wState: WriterState): Unit = {
    val thisModProps: Seq[ModuleLevelProperty] = cs.annotations.collect {
      case mlp: ModuleLevelProperty if (mlp.enclosingModule.module == m.name) => mlp
    }
    thisModProps.foreach { prop => wState.write(s"  ${prop.serializeUCL}\n") }
  }

  private case class BMC(steps: BigInt)

  private def emit_bmc(bmc: BMC)(implicit wState: WriterState) {
    val id = wState.indentString
    wState.write(
      s"""|${id}vobj = bmc(${bmc.steps});
          |${id}check;
          |${id}print_results;
          |${id}vobj.print_cex;
          |""".stripMargin
    )
  }

  private def emit_control_block(cs: CircuitState)(implicit wState: WriterState): Unit = {
    val bmc = cs.annotations.collectFirst {
      case BMCAnnotation(steps) => BMC(steps)
    }

    indent_line()
    wState.write(s"control {\n")
    wState.increaseIndent()

    bmc.foreach(bmc => emit_bmc(bmc))

    wState.decreaseIndent()
    indent_line()
    wState.write(s"}\n")
  }

  private def emit_module(m: Module, cs: CircuitState, modMap: LinkedHashMap[String, Module])(implicit wState: WriterState): Unit = {
    val consts = LinkedHashMap[String, Type]() ++ cs.annotations.collect {
      case UclidFreeConstantAnnotation(ReferenceTarget(_, mname, Nil, ref, Nil)) =>
        mname match {
          case s if (s == m.name) => (ref, UnknownType)
        }
      case malformed: UclidFreeConstantAnnotation => ??? // TODO: resolve target paths
    }

    val init_assumptions = cs.annotations.collect {
      case UclidInitialAssumptionAnnotation(rt: ReferenceTarget, value) if rt.encapsulatingModule == m.name =>
        (rt.ref, value)
    }

    // Just IO, nodes, registers
    val nodes = ArrayBuffer[DefNode]()
    val wire_decls = ArrayBuffer[DefWire]()
    val inst_decls = LinkedHashMap[String, WDefInstance]()
    val inst_fields = LinkedHashMap[String, LinkedHashMap[String, Expression]]()
    val output_decls = m.ports.filter(_.direction == Output).map(p => p.name -> p).toMap
    val clocks = HashSet[Expression]()
    //TODO: Not an efficient data structure, change to disjoint sets
    val eq_clocks = LinkedHashMap[String, HashSet[String]]()
    val reg_resets = HashSet[String]()
    val reg_decls = LinkedHashMap[String, DefRegister]()
    val mem_decls = ArrayBuffer[DefMemory]()
    val reg_assigns = ArrayBuffer[Connect]()
    val comb_assigns = ArrayBuffer[Connect]()
    val wire_assigns = ArrayBuffer[Connect]()

    def processStatements(stmt: Statement): Unit = stmt match {
      case sx: DefNode =>
        if (sx.value.tpe == ClockType) {
          sx.value match {
            case wr: WRef =>
                val clock_set = eq_clocks.getOrElseUpdate(wr.name, new HashSet[String]())
                clock_set.add(sx.name)
            case _ => throwInternalError(s"Cannot handle complex clock node def")
          }
        } else {
          nodes += sx
        }
      case sx @ DefRegister(_, name, tpe, _, Utils.zero, _) if consts.contains(name) =>
        consts(name) = tpe
      case sx: DefRegister =>
        clocks += sx.clock
        sx.reset match {
          case wr: WRef =>
            reg_resets += wr.name
          case UIntLiteral(v: BigInt, _) if (v == 0) =>
          case _ => throwInternalError(s"Illegal reset signal ${sx.reset}")
        }
        reg_decls += sx.name -> sx
      // TODO: Ensure that all connect statements are automatically sign extended.
      case sx @ Connect(_, lhs, rhs) => lhs match {
        case WRef(rName, _, RegKind, _) =>
          if (!consts.contains(rName)) {
            reg_assigns += sx
          }
        case _ =>
          kind(lhs) match {
            case RegKind => reg_assigns += sx
            case PortKind => comb_assigns += sx
            case MemKind => rhs.tpe match {
              case ClockType =>
                clocks += rhs
              case _ =>
                comb_assigns += sx
            }
            case InstanceKind => lhs match {
              case WSubField(WRef(inst_name,_,_,_), field, tpe, flow) =>
                //TODO: Does inst need to be declared at this point?
                require(inst_decls.contains(inst_name))
                if (tpe != ClockType) {
                  val fields = inst_fields.getOrElseUpdate(inst_name, new LinkedHashMap[String, Expression]())
                  //TODO: Are fields mappings unique
                  //fields += field -> rhs
                  comb_assigns += sx
                }
              case _ =>
                throwInternalError(s"Only subfields of an instance may be on the lhs of a Connect")
            }
            case _ =>
              throwInternalError(s"Only outputs, registers, and mem fields may be the lhs of Connect")
          }
      }
      case sx @ DefMemory(_, n, dt, _, wlat, rlat, rs , ws, rws, _) =>
        require(wlat == 1 && rlat == 0 && rws.size == 0, "Must run after VerilogMemDelays!")
        require(dt.isInstanceOf[GroundType], "Must run after LowerTypes")
        mem_decls += sx
        wire_decls += DefWire(NoInfo, s"havoc_$n", dt)
        for (r <- rs) {
          val data = memPortField(sx, r, "data")
          val addr = memPortField(sx, r, "addr")
          val en = memPortField(sx, r, "en")
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(data), data.tpe)
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(addr), addr.tpe)
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(en), en.tpe)
        }
        for (w <- ws) {
          val data = memPortField(sx, w, "data")
          val addr = memPortField(sx, w, "addr")
          val en = memPortField(sx, w, "en")
          val mask = memPortField(sx, w, "mask")
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(data), data.tpe)
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(addr), addr.tpe)
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(en), en.tpe)
          wire_decls += DefWire(NoInfo, LowerTypes.loweredName(mask), mask.tpe)
        }
      //TODO: Connects are being processed, we should be able to remove the connect case below
      case sx @ WDefInstance(_,name,module,_) =>
        inst_decls += sx.name -> sx
      case DefWire(_,_,_) =>
        // These are illegal for now
        throw EmitterException("Using illegal statement!")
      case sx => sx.foreach(processStatements)
    }

    // Process FIRRTL implementation
    processStatements(m.body)

    // Join equal clock sets
    //TODO: Fix this inefficient implementation
    eq_clocks.foreach({
      case (k1, set1) =>
        eq_clocks.foreach({
          case (k2, set2) =>
            if (set1.contains(k2)) {
              eq_clocks.put(k1, set1.union(set2))
              eq_clocks.remove(k2)
            }
        })
    })
    val clock_sets = new ArrayBuffer[HashSet[String]]()
    eq_clocks.foreach({
      case (k, set) =>
        set.add(k)
        clock_sets.append(set)
    })


    // Consistency checks to see if module uses <=1 clock and <=1 reset
    if (clock_sets.size > 1 || reg_resets.size > 0)
      throw EmitterException("Uclid backend supports only a single clock domain and zero explicit resets")
    wState.zeroIndent()
    emit_open_module_scope(m)
    m.ports.filter(p => p.tpe != ClockType && !reg_resets.contains(p.name)).foreach(emit_port(_))

    wState.debugComment("Free Constants")
    consts.foreach({ case (k, v) => emit_const_decl(k, v) })

    wState.debugComment("Registers")
    reg_decls.foreach({ case (k, v) => emit_reg_decl(v) })

    wState.debugComment("Memories")
    mem_decls.foreach(emit_mem_decl(_))

    wState.debugComment("Wires")
    wire_decls.foreach(emit_wire_decl(_))

    wState.debugComment("Nodes")
    nodes.foreach(emit_node_decl(_))

    wState.debugComment("Instances")
    inst_decls.foreach({
      case (k, v) =>
        val fields = inst_fields.getOrElse(k, new LinkedHashMap[String, Expression]())
        val mod = modMap.get(v.module) match {
          case Some(value) => value
          case None => throw EmitterException("A corresponding module must exist for any instance")
        }
        emit_inst_decl(v, fields, mod)
    })

    wState.debugComment("Init")
    emit_init(mem_decls, nodes, comb_assigns, init_assumptions)

    wState.debugComment("Mem Writes")
    mem_decls.foreach(emit_mem_write_procedure(_))
    emit_open_next_scope()

    wState.debugComment("Clock High")
    mem_decls.foreach(emit_mem_writes(_))
    reg_assigns.foreach(ra => emit_connect(ra, false))

    wState.debugComment("Clock Low")
    nodes.foreach(emit_node_assignment(_))
    mem_decls.foreach(emit_mem_reads(_))
    comb_assigns.foreach(comb => emit_connect(comb, rhsPrimes = true))
    inst_decls.foreach(idecl => emit_inst_next(idecl._2))
    emit_close_scope()
    emit_module_level_annos(cs, m)
    if (m.name == cs.circuit.main) {
      emit_control_block(cs)
    }
    emit_close_scope()
  }

  def emit(cs: CircuitState, wState: WriterState): Unit = {
    val instGraph = new InstanceGraph(cs.circuit)
    val moduleOrder = instGraph.moduleOrder.reverse
    // Used for creating UCLID5 instances
    val moduleMap = new LinkedHashMap[String, Module]()
    moduleOrder.foreach(m => m match {
      case m : Module => {
        moduleMap += m.name -> m
      }
      case _ =>
    })

    moduleOrder.map(m => m match {
      case m: Module => emit_module(m, cs, moduleMap)(wState)
      //TODO: Need to handle external modules
      case _ => throw EmitterException(s"UCLID backed supports ordinary modules only!")
    })
  }

  def emit(cs: CircuitState, writer: Writer): Unit = emit(cs, new WriterState(writer = writer))

  override def execute(cs: CircuitState): CircuitState = {
    val debug = cs.annotations.collectFirst({ case DebugLevel(dbg) => dbg }).getOrElse(false)
    val extraAnnotations = cs.annotations.flatMap {
      case EmitCircuitAnnotation(_) =>
        val wState = new WriterState(debugOutput = debug)
        emit(cs, wState)
        Seq(EmittedVerilogCircuitAnnotation(EmittedVerilogCircuit(cs.circuit.main, wState.serialize(), outputSuffix)))
      case _ => Seq()
    }
    cs.copy(annotations = extraAnnotations ++ cs.annotations)
  }
}
