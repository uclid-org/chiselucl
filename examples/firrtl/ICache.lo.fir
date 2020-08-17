circuit ICache :
  module ICache :
    input clock : Clock
    input reset : UInt<1>
    input io_req_valid : UInt<1>
    input io_req_bits_addr : UInt<39>
    input io_s1_paddr : UInt<32>
    input io_s1_kill : UInt<1>
    input io_s2_kill : UInt<1>
    input io_resp_ready : UInt<1>
    output io_resp_valid : UInt<1>
    output io_resp_bits_data : UInt<16>
    output io_resp_bits_datablock : UInt<64>
    input io_invalidate : UInt<1>
    input io_mem_0_a_ready : UInt<1>
    output io_mem_0_a_valid : UInt<1>
    output io_mem_0_a_bits_opcode : UInt<3>
    output io_mem_0_a_bits_param : UInt<3>
    output io_mem_0_a_bits_size : UInt<4>
    output io_mem_0_a_bits_source : UInt<1>
    output io_mem_0_a_bits_address : UInt<32>
    output io_mem_0_a_bits_mask : UInt<8>
    output io_mem_0_a_bits_data : UInt<64>
    output io_mem_0_b_ready : UInt<1>
    input io_mem_0_b_valid : UInt<1>
    input io_mem_0_b_bits_opcode : UInt<3>
    input io_mem_0_b_bits_param : UInt<2>
    input io_mem_0_b_bits_size : UInt<4>
    input io_mem_0_b_bits_source : UInt<1>
    input io_mem_0_b_bits_address : UInt<32>
    input io_mem_0_b_bits_mask : UInt<8>
    input io_mem_0_b_bits_data : UInt<64>
    input io_mem_0_c_ready : UInt<1>
    output io_mem_0_c_valid : UInt<1>
    output io_mem_0_c_bits_opcode : UInt<3>
    output io_mem_0_c_bits_param : UInt<3>
    output io_mem_0_c_bits_size : UInt<4>
    output io_mem_0_c_bits_source : UInt<1>
    output io_mem_0_c_bits_address : UInt<32>
    output io_mem_0_c_bits_data : UInt<64>
    output io_mem_0_c_bits_error : UInt<1>
    output io_mem_0_d_ready : UInt<1>
    input io_mem_0_d_valid : UInt<1>
    input io_mem_0_d_bits_opcode : UInt<3>
    input io_mem_0_d_bits_param : UInt<2>
    input io_mem_0_d_bits_size : UInt<4>
    input io_mem_0_d_bits_source : UInt<1>
    input io_mem_0_d_bits_sink : UInt<4>
    input io_mem_0_d_bits_addr_lo : UInt<3>
    input io_mem_0_d_bits_data : UInt<64>
    input io_mem_0_d_bits_error : UInt<1>
    input io_mem_0_e_ready : UInt<1>
    output io_mem_0_e_valid : UInt<1>
    output io_mem_0_e_bits_sink : UInt<4>
  
    mem tag_array_0 : @[ICache.scala 97:25]
      data-type => UInt<20>
      depth => 64
      read-latency => 0
      write-latency => 1
      reader => tag_rdata
      writer => _T_328
      read-under-write => undefined
    mem tag_array_1 : @[ICache.scala 97:25]
      data-type => UInt<20>
      depth => 64
      read-latency => 0
      write-latency => 1
      reader => tag_rdata
      writer => _T_328
      read-under-write => undefined
    mem tag_array_2 : @[ICache.scala 97:25]
      data-type => UInt<20>
      depth => 64
      read-latency => 0
      write-latency => 1
      reader => tag_rdata
      writer => _T_328
      read-under-write => undefined
    mem tag_array_3 : @[ICache.scala 97:25]
      data-type => UInt<20>
      depth => 64
      read-latency => 0
      write-latency => 1
      reader => tag_rdata
      writer => _T_328
      read-under-write => undefined
    mem _T_550 : @[ICache.scala 132:28]
      data-type => UInt<64>
      depth => 512
      read-latency => 0
      write-latency => 1
      reader => _T_566
      writer => _T_556
      read-under-write => undefined
    mem _T_572 : @[ICache.scala 132:28]
      data-type => UInt<64>
      depth => 512
      read-latency => 0
      write-latency => 1
      reader => _T_588
      writer => _T_578
      read-under-write => undefined
    mem _T_594 : @[ICache.scala 132:28]
      data-type => UInt<64>
      depth => 512
      read-latency => 0
      write-latency => 1
      reader => _T_610
      writer => _T_600
      read-under-write => undefined
    mem _T_616 : @[ICache.scala 132:28]
      data-type => UInt<64>
      depth => 512
      read-latency => 0
      write-latency => 1
      reader => _T_632
      writer => _T_622
      read-under-write => undefined
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[ICache.scala 67:18]
    reg invalidated : UInt<1>, clock with :
      reset => (UInt<1>("h0"), invalidated) @[ICache.scala 68:24]
    node stall = eq(io_resp_ready, UInt<1>("h0")) @[ICache.scala 69:15]
    reg refill_addr : UInt<32>, clock with :
      reset => (UInt<1>("h0"), refill_addr) @[ICache.scala 71:24]
    reg s1_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s1_valid) @[ICache.scala 74:21]
    node _T_221 = eq(io_s1_kill, UInt<1>("h0")) @[ICache.scala 75:31]
    node _T_222 = and(s1_valid, _T_221) @[ICache.scala 75:28]
    node _T_223 = eq(state, UInt<2>("h0")) @[ICache.scala 75:52]
    node out_valid = and(_T_222, _T_223) @[ICache.scala 75:43]
    node s1_idx = bits(io_s1_paddr, 11, 6) @[ICache.scala 76:27]
    node s1_tag = bits(io_s1_paddr, 31, 12) @[ICache.scala 77:27]
    node _T_436 = eq(io_invalidate, UInt<1>("h0")) @[ICache.scala 122:17]
    reg vb_array : UInt<256>, clock with :
      reset => (UInt<1>("h0"), vb_array) @[ICache.scala 104:21]
    skip
    node _T_439 = cat(UInt<1>("h0"), s1_idx) @[Cat.scala 30:58]
    node _T_440 = dshr(vb_array, _T_439) @[ICache.scala 122:43]
    node _T_441 = bits(_T_440, 0, 0) @[ICache.scala 122:43]
    skip
    node _T_443 = and(_T_436, _T_441) @[ICache.scala 122:32]
    reg s1_dout_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s1_dout_valid) @[Reg.scala 14:44]
    node _T_450 = tag_array_0.tag_rdata.data @[ICache.scala 125:32]
    node _T_451 = eq(_T_450, s1_tag) @[ICache.scala 125:46]
    reg _T_453 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_453) @[Reg.scala 34:16]
    node s1_tag_match_0 = mux(s1_dout_valid, _T_451, _T_453) @[Package.scala 27:42]
    skip
    node s1_tag_hit_0 = and(_T_443, s1_tag_match_0) @[ICache.scala 126:28]
    skip
    skip
    skip
    node _T_465 = cat(UInt<1>("h1"), s1_idx) @[Cat.scala 30:58]
    node _T_466 = dshr(vb_array, _T_465) @[ICache.scala 122:43]
    node _T_467 = bits(_T_466, 0, 0) @[ICache.scala 122:43]
    skip
    node _T_469 = and(_T_436, _T_467) @[ICache.scala 122:32]
    node _T_476 = tag_array_1.tag_rdata.data @[ICache.scala 125:32]
    node _T_477 = eq(_T_476, s1_tag) @[ICache.scala 125:46]
    reg _T_479 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_479) @[Reg.scala 34:16]
    node s1_tag_match_1 = mux(s1_dout_valid, _T_477, _T_479) @[Package.scala 27:42]
    skip
    node s1_tag_hit_1 = and(_T_469, s1_tag_match_1) @[ICache.scala 126:28]
    skip
    node _T_539 = or(s1_tag_hit_0, s1_tag_hit_1) @[ICache.scala 129:44]
    skip
    skip
    node _T_491 = cat(UInt<2>("h2"), s1_idx) @[Cat.scala 30:58]
    node _T_492 = dshr(vb_array, _T_491) @[ICache.scala 122:43]
    node _T_493 = bits(_T_492, 0, 0) @[ICache.scala 122:43]
    skip
    node _T_495 = and(_T_436, _T_493) @[ICache.scala 122:32]
    node _T_502 = tag_array_2.tag_rdata.data @[ICache.scala 125:32]
    node _T_503 = eq(_T_502, s1_tag) @[ICache.scala 125:46]
    reg _T_505 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_505) @[Reg.scala 34:16]
    node s1_tag_match_2 = mux(s1_dout_valid, _T_503, _T_505) @[Package.scala 27:42]
    skip
    node s1_tag_hit_2 = and(_T_495, s1_tag_match_2) @[ICache.scala 126:28]
    skip
    node _T_540 = or(_T_539, s1_tag_hit_2) @[ICache.scala 129:44]
    skip
    skip
    node _T_517 = cat(UInt<2>("h3"), s1_idx) @[Cat.scala 30:58]
    node _T_518 = dshr(vb_array, _T_517) @[ICache.scala 122:43]
    node _T_519 = bits(_T_518, 0, 0) @[ICache.scala 122:43]
    skip
    node _T_521 = and(_T_436, _T_519) @[ICache.scala 122:32]
    node _T_528 = tag_array_3.tag_rdata.data @[ICache.scala 125:32]
    node _T_529 = eq(_T_528, s1_tag) @[ICache.scala 125:46]
    reg _T_531 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_531) @[Reg.scala 34:16]
    node s1_tag_match_3 = mux(s1_dout_valid, _T_529, _T_531) @[Package.scala 27:42]
    skip
    node s1_tag_hit_3 = and(_T_521, s1_tag_match_3) @[ICache.scala 126:28]
    skip
    node s1_any_tag_hit = or(_T_540, s1_tag_hit_3) @[ICache.scala 129:44]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node s1_hit = and(out_valid, s1_any_tag_hit) @[ICache.scala 78:26]
    node _T_225 = eq(s1_any_tag_hit, UInt<1>("h0")) @[ICache.scala 79:30]
    node s1_miss = and(out_valid, _T_225) @[ICache.scala 79:27]
    skip
    node _T_227 = and(io_req_valid, _T_223) @[ICache.scala 81:31]
    node _T_228 = and(out_valid, stall) @[ICache.scala 81:67]
    node _T_230 = eq(_T_228, UInt<1>("h0")) @[ICache.scala 81:55]
    node s0_valid = and(_T_227, _T_230) @[ICache.scala 81:52]
    skip
    node _T_232 = or(s0_valid, _T_228) @[ICache.scala 84:24]
    skip
    node _T_234 = and(s1_miss, _T_223) @[ICache.scala 86:17]
    skip
    skip
    node refill_idx = bits(refill_addr, 11, 6) @[ICache.scala 90:31]
    node _T_235 = and(io_mem_0_d_ready, io_mem_0_d_valid) @[Decoupled.scala 30:37]
    skip
    node _T_238 = dshlw(UInt<23>("hff"), io_mem_0_d_bits_size) @[package.scala 19:71]
    node _T_239 = bits(_T_238, 7, 0) @[package.scala 19:76]
    node _T_240 = not(_T_239) @[package.scala 19:40]
    node _T_241 = shr(_T_240, 3) @[Edges.scala 198:59]
    node _T_242 = bits(io_mem_0_d_bits_opcode, 0, 0) @[Edges.scala 90:36]
    node _T_244 = mux(_T_242, _T_241, UInt<5>("h0")) @[Edges.scala 199:14]
    reg _T_246 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), _T_246) @[Reg.scala 26:44]
    node _T_248 = sub(_T_246, UInt<5>("h1")) @[Edges.scala 208:28]
    node _T_249 = asUInt(_T_248) @[Edges.scala 208:28]
    node _T_250 = tail(_T_249, 1) @[Edges.scala 208:28]
    node _T_252 = eq(_T_246, UInt<5>("h0")) @[Edges.scala 209:25]
    node _T_254 = eq(_T_246, UInt<5>("h1")) @[Edges.scala 210:25]
    node _T_256 = eq(_T_244, UInt<5>("h0")) @[Edges.scala 210:47]
    node _T_257 = or(_T_254, _T_256) @[Edges.scala 210:37]
    node refill_done = and(_T_257, _T_235) @[Edges.scala 211:22]
    node _T_258 = not(_T_250) @[Edges.scala 212:27]
    node refill_cnt = and(_T_244, _T_258) @[Edges.scala 212:25]
    node _T_259 = mux(_T_252, _T_244, _T_250) @[Edges.scala 214:21]
    node _GEN_1 = mux(_T_235, _T_259, _T_246) @[Edges.scala 213:17]
    reg _T_262 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), _T_262) @[LFSR.scala 22:19]
    node _T_263 = bits(_T_262, 0, 0) @[LFSR.scala 23:40]
    node _T_264 = bits(_T_262, 2, 2) @[LFSR.scala 23:48]
    node _T_265 = xor(_T_263, _T_264) @[LFSR.scala 23:43]
    node _T_266 = bits(_T_262, 3, 3) @[LFSR.scala 23:56]
    node _T_267 = xor(_T_265, _T_266) @[LFSR.scala 23:51]
    node _T_268 = bits(_T_262, 5, 5) @[LFSR.scala 23:64]
    node _T_269 = xor(_T_267, _T_268) @[LFSR.scala 23:59]
    node _T_270 = bits(_T_262, 15, 1) @[LFSR.scala 23:73]
    node _T_271 = cat(_T_269, _T_270) @[Cat.scala 30:58]
    node _GEN_2 = mux(s1_miss, _T_271, _T_262) @[LFSR.scala 23:22]
    node repl_way = bits(_T_262, 1, 0) @[ICache.scala 95:56]
    node _T_282 = bits(io_req_bits_addr, 11, 6) @[ICache.scala 98:42]
    node _T_284 = eq(refill_done, UInt<1>("h0")) @[ICache.scala 98:70]
    node _T_285 = and(_T_284, s0_valid) @[ICache.scala 98:83]
    skip
    skip
    skip
    skip
    node _GEN_4 = _T_285
    skip
    skip
    node _T_312 = eq(repl_way, UInt<2>("h0")) @[ICache.scala 101:84]
    node _T_314 = eq(repl_way, UInt<2>("h1")) @[ICache.scala 101:84]
    node _T_316 = eq(repl_way, UInt<2>("h2")) @[ICache.scala 101:84]
    node _T_318 = eq(repl_way, UInt<2>("h3")) @[ICache.scala 101:84]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_342 = eq(invalidated, UInt<1>("h0")) @[ICache.scala 105:24]
    node _T_343 = and(refill_done, _T_342) @[ICache.scala 105:21]
    node _T_344 = cat(repl_way, refill_idx) @[Cat.scala 30:58]
    node _T_347 = dshlw(UInt<256>("h1"), _T_344) @[ICache.scala 106:32]
    node _T_348 = or(vb_array, _T_347) @[ICache.scala 106:32]
    skip
    skip
    skip
    skip
    node _GEN_26 = mux(_T_343, _T_348, vb_array) @[ICache.scala 105:38]
    node _GEN_27 = mux(io_invalidate, UInt<256>("h0"), _GEN_26) @[ICache.scala 108:24]
    node _GEN_28 = or(io_invalidate, invalidated) @[ICache.scala 108:24]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_553 = and(io_mem_0_d_valid, _T_312) @[ICache.scala 133:30]
    node _T_554 = shl(refill_idx, 3) @[ICache.scala 136:36]
    node _GEN_152 = pad(refill_cnt, 9) @[ICache.scala 136:63]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_557 = bits(io_req_bits_addr, 11, 3) @[ICache.scala 138:28]
    node _T_559 = eq(_T_553, UInt<1>("h0")) @[ICache.scala 139:45]
    node _T_560 = and(_T_559, s0_valid) @[ICache.scala 139:50]
    skip
    skip
    skip
    skip
    node _GEN_47 = _T_560
    skip
    skip
    reg _T_568 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_568) @[Reg.scala 34:16]
    node _GEN_50 = mux(s1_dout_valid, _T_550._T_566.data, _T_568) @[Reg.scala 35:19]
    skip
    skip
    node _T_575 = and(io_mem_0_d_valid, _T_314) @[ICache.scala 133:30]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_581 = eq(_T_575, UInt<1>("h0")) @[ICache.scala 139:45]
    node _T_582 = and(_T_581, s0_valid) @[ICache.scala 139:50]
    skip
    skip
    skip
    skip
    node _GEN_57 = _T_582
    skip
    skip
    reg _T_590 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_590) @[Reg.scala 34:16]
    node _GEN_60 = mux(s1_dout_valid, _T_572._T_588.data, _T_590) @[Reg.scala 35:19]
    skip
    skip
    node _T_597 = and(io_mem_0_d_valid, _T_316) @[ICache.scala 133:30]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_603 = eq(_T_597, UInt<1>("h0")) @[ICache.scala 139:45]
    node _T_604 = and(_T_603, s0_valid) @[ICache.scala 139:50]
    skip
    skip
    skip
    skip
    node _GEN_67 = _T_604
    skip
    skip
    reg _T_612 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_612) @[Reg.scala 34:16]
    node _GEN_70 = mux(s1_dout_valid, _T_594._T_610.data, _T_612) @[Reg.scala 35:19]
    skip
    skip
    node _T_619 = and(io_mem_0_d_valid, _T_318) @[ICache.scala 133:30]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_625 = eq(_T_619, UInt<1>("h0")) @[ICache.scala 139:45]
    node _T_626 = and(_T_625, s0_valid) @[ICache.scala 139:50]
    skip
    skip
    skip
    skip
    node _GEN_77 = _T_626
    skip
    skip
    reg _T_634 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_634) @[Reg.scala 34:16]
    node _GEN_80 = mux(s1_dout_valid, _T_616._T_632.data, _T_634) @[Reg.scala 35:19]
    skip
    node _T_638 = eq(stall, UInt<1>("h0")) @[ICache.scala 148:51]
    reg _T_639 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_639) @[Reg.scala 26:44]
    node _GEN_81 = mux(_T_638, s1_hit, _T_639) @[Reg.scala 43:19]
    skip
    reg _T_654_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_654_0) @[Reg.scala 34:16]
    reg _T_654_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_654_1) @[Reg.scala 34:16]
    reg _T_654_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_654_2) @[Reg.scala 34:16]
    reg _T_654_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_654_3) @[Reg.scala 34:16]
    skip
    skip
    skip
    skip
    skip
    reg _T_685_0 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_685_0) @[Reg.scala 34:16]
    reg _T_685_1 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_685_1) @[Reg.scala 34:16]
    reg _T_685_2 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_685_2) @[Reg.scala 34:16]
    reg _T_685_3 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_685_3) @[Reg.scala 34:16]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_703 = mux(_T_654_0, _T_685_0, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_705 = mux(_T_654_1, _T_685_1, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_707 = mux(_T_654_2, _T_685_2, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_709 = mux(_T_654_3, _T_685_3, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_711 = or(_T_703, _T_705) @[Mux.scala 19:72]
    node _T_712 = or(_T_711, _T_707) @[Mux.scala 19:72]
    skip
    node _T_716 = eq(state, UInt<2>("h1")) @[ICache.scala 154:27]
    node _T_718 = eq(io_s2_kill, UInt<1>("h0")) @[ICache.scala 154:44]
    skip
    node _T_721 = shr(refill_addr, 6) @[ICache.scala 157:46]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_848 = eq(UInt<2>("h0"), state) @[Conditional.scala 29:28]
    node _GEN_90 = mux(s1_miss, UInt<2>("h1"), state) @[ICache.scala 165:22]
    node _GEN_91 = mux(_T_848, _GEN_90, state) @[Conditional.scala 29:59]
    skip
    node _T_850 = eq(UInt<2>("h1"), state) @[Conditional.scala 29:28]
    node _GEN_93 = mux(io_mem_0_a_ready, UInt<2>("h2"), _GEN_91) @[ICache.scala 169:29]
    node _GEN_94 = mux(io_s2_kill, UInt<2>("h0"), _GEN_93) @[ICache.scala 170:25]
    node _GEN_95 = mux(_T_850, _GEN_94, _GEN_91) @[Conditional.scala 29:59]
    node _T_851 = eq(UInt<2>("h2"), state) @[Conditional.scala 29:28]
    node _GEN_96 = mux(io_mem_0_d_valid, UInt<2>("h3"), _GEN_95) @[ICache.scala 173:29]
    node _GEN_97 = mux(_T_851, _GEN_96, _GEN_95) @[Conditional.scala 29:59]
    node _T_852 = eq(UInt<2>("h3"), state) @[Conditional.scala 29:28]
    node _GEN_98 = mux(refill_done, UInt<2>("h0"), _GEN_97) @[ICache.scala 176:26]
    node _GEN_99 = mux(_T_852, _GEN_98, _GEN_97) @[Conditional.scala 29:59]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    io_resp_valid <= _T_639 @[ICache.scala 152:21]
    io_resp_bits_data <= UInt<16>("h0")
    io_resp_bits_datablock <= or(_T_712, _T_709) @[ICache.scala 151:30]
    io_mem_0_a_valid <= and(_T_716, _T_718) @[ICache.scala 154:18]
    io_mem_0_a_bits_opcode <= UInt<3>("h4") @[ICache.scala 155:17]
    io_mem_0_a_bits_param <= UInt<3>("h0") @[ICache.scala 155:17]
    io_mem_0_a_bits_size <= UInt<4>("h6") @[ICache.scala 155:17]
    io_mem_0_a_bits_source <= UInt<1>("h0") @[ICache.scala 155:17]
    io_mem_0_a_bits_address <= shl(_T_721, 6) @[ICache.scala 155:17]
    io_mem_0_a_bits_mask <= UInt<8>("hff") @[ICache.scala 155:17]
    io_mem_0_a_bits_data <= UInt<64>("h0") @[ICache.scala 155:17]
    io_mem_0_b_ready <= UInt<1>("h0")
    io_mem_0_c_valid <= UInt<1>("h0") @[ICache.scala 159:18]
    io_mem_0_c_bits_opcode <= UInt<3>("h0")
    io_mem_0_c_bits_param <= UInt<3>("h0")
    io_mem_0_c_bits_size <= UInt<4>("h0")
    io_mem_0_c_bits_source <= UInt<1>("h0")
    io_mem_0_c_bits_address <= UInt<32>("h0")
    io_mem_0_c_bits_data <= UInt<64>("h0")
    io_mem_0_c_bits_error <= UInt<1>("h0")
    io_mem_0_d_ready <= UInt<1>("h1") @[ICache.scala 92:18]
    io_mem_0_e_valid <= UInt<1>("h0") @[ICache.scala 160:18]
    io_mem_0_e_bits_sink <= UInt<4>("h0")
    state <= mux(reset, UInt<2>("h0"), _GEN_99) @[ICache.scala 165:30 ICache.scala 169:37 ICache.scala 170:33 ICache.scala 173:37 ICache.scala 176:34]
    invalidated <= mux(_T_848, UInt<1>("h0"), _GEN_28) @[ICache.scala 110:17 ICache.scala 166:19]
    refill_addr <= mux(_T_234, io_s1_paddr, refill_addr) @[ICache.scala 87:17]
    s1_valid <= mux(reset, UInt<1>("h0"), _T_232) @[ICache.scala 84:12]
    _T_246 <= mux(reset, UInt<5>("h0"), _GEN_1) @[Edges.scala 214:15]
    _T_262 <= mux(reset, UInt<16>("h1"), _GEN_2) @[LFSR.scala 23:29]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    vb_array <= mux(reset, UInt<256>("h0"), _GEN_27) @[ICache.scala 106:14 ICache.scala 109:14 ICache.scala 114:51 ICache.scala 114:51 ICache.scala 114:51 ICache.scala 114:51]
    s1_dout_valid <= and(_T_227, _T_230) @[Reg.scala 14:44]
    skip
    _T_453 <= mux(s1_dout_valid, _T_451, _T_453) @[Reg.scala 35:23]
    skip
    _T_479 <= mux(s1_dout_valid, _T_477, _T_479) @[Reg.scala 35:23]
    skip
    _T_505 <= mux(s1_dout_valid, _T_503, _T_505) @[Reg.scala 35:23]
    skip
    _T_531 <= mux(s1_dout_valid, _T_529, _T_531) @[Reg.scala 35:23]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    _T_568 <= mux(s1_dout_valid, _T_550._T_566.data, _T_568) @[Reg.scala 35:23]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    _T_590 <= mux(s1_dout_valid, _T_572._T_588.data, _T_590) @[Reg.scala 35:23]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    _T_612 <= mux(s1_dout_valid, _T_594._T_610.data, _T_612) @[Reg.scala 35:23]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    _T_634 <= mux(s1_dout_valid, _T_616._T_632.data, _T_634) @[Reg.scala 35:23]
    _T_639 <= mux(reset, UInt<1>("h0"), _GEN_81) @[Reg.scala 43:23]
    _T_654_0 <= mux(_T_638, s1_tag_hit_0, _T_654_0) @[Reg.scala 35:23]
    _T_654_1 <= mux(_T_638, s1_tag_hit_1, _T_654_1) @[Reg.scala 35:23]
    _T_654_2 <= mux(_T_638, s1_tag_hit_2, _T_654_2) @[Reg.scala 35:23]
    _T_654_3 <= mux(_T_638, s1_tag_hit_3, _T_654_3) @[Reg.scala 35:23]
    _T_685_0 <= mux(_T_638, _GEN_50, _T_685_0) @[Reg.scala 35:23]
    _T_685_1 <= mux(_T_638, _GEN_60, _T_685_1) @[Reg.scala 35:23]
    _T_685_2 <= mux(_T_638, _GEN_70, _T_685_2) @[Reg.scala 35:23]
    _T_685_3 <= mux(_T_638, _GEN_80, _T_685_3) @[Reg.scala 35:23]
    skip
    skip
    reg tag_array_0_tag_rdata_addr_pipe_0 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), tag_array_0_tag_rdata_addr_pipe_0)
    skip
    tag_array_0_tag_rdata_addr_pipe_0 <= mux(_GEN_4, _T_282, tag_array_0_tag_rdata_addr_pipe_0)
    tag_array_0.tag_rdata.clk <= clock
    tag_array_0.tag_rdata.en <= and(_T_284, s0_valid)
    tag_array_0.tag_rdata.addr <= tag_array_0_tag_rdata_addr_pipe_0
    skip
    skip
    skip
    skip
    tag_array_0._T_328.clk <= clock
    tag_array_0._T_328.en <= and(_T_257, _T_235)
    tag_array_0._T_328.mask <= eq(repl_way, UInt<2>("h0"))
    tag_array_0._T_328.addr <= bits(refill_addr, 11, 6)
    tag_array_0._T_328.data <= bits(refill_addr, 31, 12)
    skip
    skip
    reg tag_array_1_tag_rdata_addr_pipe_0 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), tag_array_1_tag_rdata_addr_pipe_0)
    skip
    tag_array_1_tag_rdata_addr_pipe_0 <= mux(_GEN_4, _T_282, tag_array_1_tag_rdata_addr_pipe_0)
    tag_array_1.tag_rdata.clk <= clock
    tag_array_1.tag_rdata.en <= and(_T_284, s0_valid)
    tag_array_1.tag_rdata.addr <= tag_array_1_tag_rdata_addr_pipe_0
    skip
    skip
    skip
    skip
    tag_array_1._T_328.clk <= clock
    tag_array_1._T_328.en <= and(_T_257, _T_235)
    tag_array_1._T_328.mask <= eq(repl_way, UInt<2>("h1"))
    tag_array_1._T_328.addr <= bits(refill_addr, 11, 6)
    tag_array_1._T_328.data <= bits(refill_addr, 31, 12)
    skip
    skip
    reg tag_array_2_tag_rdata_addr_pipe_0 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), tag_array_2_tag_rdata_addr_pipe_0)
    skip
    tag_array_2_tag_rdata_addr_pipe_0 <= mux(_GEN_4, _T_282, tag_array_2_tag_rdata_addr_pipe_0)
    tag_array_2.tag_rdata.clk <= clock
    tag_array_2.tag_rdata.en <= and(_T_284, s0_valid)
    tag_array_2.tag_rdata.addr <= tag_array_2_tag_rdata_addr_pipe_0
    skip
    skip
    skip
    skip
    tag_array_2._T_328.clk <= clock
    tag_array_2._T_328.en <= and(_T_257, _T_235)
    tag_array_2._T_328.mask <= eq(repl_way, UInt<2>("h2"))
    tag_array_2._T_328.addr <= bits(refill_addr, 11, 6)
    tag_array_2._T_328.data <= bits(refill_addr, 31, 12)
    skip
    skip
    reg tag_array_3_tag_rdata_addr_pipe_0 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), tag_array_3_tag_rdata_addr_pipe_0)
    skip
    tag_array_3_tag_rdata_addr_pipe_0 <= mux(_GEN_4, _T_282, tag_array_3_tag_rdata_addr_pipe_0)
    tag_array_3.tag_rdata.clk <= clock
    tag_array_3.tag_rdata.en <= and(_T_284, s0_valid)
    tag_array_3.tag_rdata.addr <= tag_array_3_tag_rdata_addr_pipe_0
    skip
    skip
    skip
    skip
    tag_array_3._T_328.clk <= clock
    tag_array_3._T_328.en <= and(_T_257, _T_235)
    tag_array_3._T_328.mask <= eq(repl_way, UInt<2>("h3"))
    tag_array_3._T_328.addr <= bits(refill_addr, 11, 6)
    tag_array_3._T_328.data <= bits(refill_addr, 31, 12)
    skip
    skip
    reg _T_550__T_566_addr_pipe_0 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_550__T_566_addr_pipe_0)
    skip
    _T_550__T_566_addr_pipe_0 <= mux(_GEN_47, _T_557, _T_550__T_566_addr_pipe_0)
    _T_550._T_566.clk <= clock
    _T_550._T_566.en <= and(_T_559, s0_valid)
    _T_550._T_566.addr <= _T_550__T_566_addr_pipe_0
    skip
    skip
    skip
    skip
    skip
    _T_550._T_556.clk <= clock
    _T_550._T_556.en <= and(io_mem_0_d_valid, _T_312)
    _T_550._T_556.mask <= UInt<1>("h1")
    skip
    _T_550._T_556.addr <= or(_T_554, _GEN_152)
    _T_550._T_556.data <= io_mem_0_d_bits_data
    skip
    skip
    reg _T_572__T_588_addr_pipe_0 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_572__T_588_addr_pipe_0)
    skip
    _T_572__T_588_addr_pipe_0 <= mux(_GEN_57, _T_557, _T_572__T_588_addr_pipe_0)
    _T_572._T_588.clk <= clock
    _T_572._T_588.en <= and(_T_581, s0_valid)
    _T_572._T_588.addr <= _T_572__T_588_addr_pipe_0
    skip
    skip
    skip
    skip
    skip
    _T_572._T_578.clk <= clock
    _T_572._T_578.en <= and(io_mem_0_d_valid, _T_314)
    _T_572._T_578.mask <= UInt<1>("h1")
    skip
    _T_572._T_578.addr <= or(_T_554, _GEN_152)
    _T_572._T_578.data <= io_mem_0_d_bits_data
    skip
    skip
    reg _T_594__T_610_addr_pipe_0 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_594__T_610_addr_pipe_0)
    skip
    _T_594__T_610_addr_pipe_0 <= mux(_GEN_67, _T_557, _T_594__T_610_addr_pipe_0)
    _T_594._T_610.clk <= clock
    _T_594._T_610.en <= and(_T_603, s0_valid)
    _T_594._T_610.addr <= _T_594__T_610_addr_pipe_0
    skip
    skip
    skip
    skip
    skip
    _T_594._T_600.clk <= clock
    _T_594._T_600.en <= and(io_mem_0_d_valid, _T_316)
    _T_594._T_600.mask <= UInt<1>("h1")
    skip
    _T_594._T_600.addr <= or(_T_554, _GEN_152)
    _T_594._T_600.data <= io_mem_0_d_bits_data
    skip
    skip
    reg _T_616__T_632_addr_pipe_0 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_616__T_632_addr_pipe_0)
    skip
    _T_616__T_632_addr_pipe_0 <= mux(_GEN_77, _T_557, _T_616__T_632_addr_pipe_0)
    _T_616._T_632.clk <= clock
    _T_616._T_632.en <= and(_T_625, s0_valid)
    _T_616._T_632.addr <= _T_616__T_632_addr_pipe_0
    skip
    skip
    skip
    skip
    skip
    _T_616._T_622.clk <= clock
    _T_616._T_622.en <= and(io_mem_0_d_valid, _T_318)
    _T_616._T_622.mask <= UInt<1>("h1")
    skip
    _T_616._T_622.addr <= or(_T_554, _GEN_152)
    _T_616._T_622.data <= io_mem_0_d_bits_data
