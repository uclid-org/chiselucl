circuit RocketCore :
  module RocketCore :
    input clock : Clock
    input reset : UInt<1>
    input io_interrupts_debug : UInt<1>
    input io_interrupts_mtip : UInt<1>
    input io_interrupts_msip : UInt<1>
    input io_interrupts_meip : UInt<1>
    input io_interrupts_seip : UInt<1>
    input io_hartid : UInt<64>
    output io_imem_req_valid : UInt<1>
    output io_imem_req_bits_pc : UInt<40>
    output io_imem_req_bits_speculative : UInt<1>
    output io_imem_resp_ready : UInt<1>
    input io_imem_resp_valid : UInt<1>
    input io_imem_resp_bits_btb_valid : UInt<1>
    input io_imem_resp_bits_btb_bits_taken : UInt<1>
    input io_imem_resp_bits_btb_bits_mask : UInt<2>
    input io_imem_resp_bits_btb_bits_bridx : UInt<1>
    input io_imem_resp_bits_btb_bits_target : UInt<39>
    input io_imem_resp_bits_btb_bits_entry : UInt<6>
    input io_imem_resp_bits_btb_bits_bht_history : UInt<7>
    input io_imem_resp_bits_btb_bits_bht_value : UInt<2>
    input io_imem_resp_bits_pc : UInt<40>
    input io_imem_resp_bits_data : UInt<32>
    input io_imem_resp_bits_mask : UInt<2>
    input io_imem_resp_bits_xcpt_if : UInt<1>
    input io_imem_resp_bits_replay : UInt<1>
    output io_imem_btb_update_valid : UInt<1>
    output io_imem_btb_update_bits_prediction_valid : UInt<1>
    output io_imem_btb_update_bits_prediction_bits_taken : UInt<1>
    output io_imem_btb_update_bits_prediction_bits_mask : UInt<2>
    output io_imem_btb_update_bits_prediction_bits_bridx : UInt<1>
    output io_imem_btb_update_bits_prediction_bits_target : UInt<39>
    output io_imem_btb_update_bits_prediction_bits_entry : UInt<6>
    output io_imem_btb_update_bits_prediction_bits_bht_history : UInt<7>
    output io_imem_btb_update_bits_prediction_bits_bht_value : UInt<2>
    output io_imem_btb_update_bits_pc : UInt<39>
    output io_imem_btb_update_bits_target : UInt<39>
    output io_imem_btb_update_bits_taken : UInt<1>
    output io_imem_btb_update_bits_isValid : UInt<1>
    output io_imem_btb_update_bits_isJump : UInt<1>
    output io_imem_btb_update_bits_isReturn : UInt<1>
    output io_imem_btb_update_bits_br_pc : UInt<39>
    output io_imem_bht_update_valid : UInt<1>
    output io_imem_bht_update_bits_prediction_valid : UInt<1>
    output io_imem_bht_update_bits_prediction_bits_taken : UInt<1>
    output io_imem_bht_update_bits_prediction_bits_mask : UInt<2>
    output io_imem_bht_update_bits_prediction_bits_bridx : UInt<1>
    output io_imem_bht_update_bits_prediction_bits_target : UInt<39>
    output io_imem_bht_update_bits_prediction_bits_entry : UInt<6>
    output io_imem_bht_update_bits_prediction_bits_bht_history : UInt<7>
    output io_imem_bht_update_bits_prediction_bits_bht_value : UInt<2>
    output io_imem_bht_update_bits_pc : UInt<39>
    output io_imem_bht_update_bits_taken : UInt<1>
    output io_imem_bht_update_bits_mispredict : UInt<1>
    output io_imem_ras_update_valid : UInt<1>
    output io_imem_ras_update_bits_isCall : UInt<1>
    output io_imem_ras_update_bits_isReturn : UInt<1>
    output io_imem_ras_update_bits_returnAddr : UInt<39>
    output io_imem_ras_update_bits_prediction_valid : UInt<1>
    output io_imem_ras_update_bits_prediction_bits_taken : UInt<1>
    output io_imem_ras_update_bits_prediction_bits_mask : UInt<2>
    output io_imem_ras_update_bits_prediction_bits_bridx : UInt<1>
    output io_imem_ras_update_bits_prediction_bits_target : UInt<39>
    output io_imem_ras_update_bits_prediction_bits_entry : UInt<6>
    output io_imem_ras_update_bits_prediction_bits_bht_history : UInt<7>
    output io_imem_ras_update_bits_prediction_bits_bht_value : UInt<2>
    output io_imem_flush_icache : UInt<1>
    output io_imem_flush_tlb : UInt<1>
    input io_imem_npc : UInt<40>
    input io_imem_acquire : UInt<1>
    input io_dmem_req_ready : UInt<1>
    output io_dmem_req_valid : UInt<1>
    output io_dmem_req_bits_addr : UInt<40>
    output io_dmem_req_bits_tag : UInt<7>
    output io_dmem_req_bits_cmd : UInt<5>
    output io_dmem_req_bits_typ : UInt<3>
    output io_dmem_req_bits_phys : UInt<1>
    output io_dmem_req_bits_data : UInt<64>
    output io_dmem_s1_kill : UInt<1>
    output io_dmem_s1_data : UInt<64>
    input io_dmem_s2_nack : UInt<1>
    input io_dmem_acquire : UInt<1>
    input io_dmem_release : UInt<1>
    input io_dmem_resp_valid : UInt<1>
    input io_dmem_resp_bits_addr : UInt<40>
    input io_dmem_resp_bits_tag : UInt<7>
    input io_dmem_resp_bits_cmd : UInt<5>
    input io_dmem_resp_bits_typ : UInt<3>
    input io_dmem_resp_bits_data : UInt<64>
    input io_dmem_resp_bits_replay : UInt<1>
    input io_dmem_resp_bits_has_data : UInt<1>
    input io_dmem_resp_bits_data_word_bypass : UInt<64>
    input io_dmem_resp_bits_store_data : UInt<64>
    input io_dmem_replay_next : UInt<1>
    input io_dmem_xcpt_ma_ld : UInt<1>
    input io_dmem_xcpt_ma_st : UInt<1>
    input io_dmem_xcpt_pf_ld : UInt<1>
    input io_dmem_xcpt_pf_st : UInt<1>
    output io_dmem_invalidate_lr : UInt<1>
    input io_dmem_ordered : UInt<1>
    output io_ptw_ptbr_mode : UInt<4>
    output io_ptw_ptbr_asid : UInt<16>
    output io_ptw_ptbr_ppn : UInt<44>
    output io_ptw_invalidate : UInt<1>
    output io_ptw_status_debug : UInt<1>
    output io_ptw_status_isa : UInt<32>
    output io_ptw_status_prv : UInt<2>
    output io_ptw_status_sd : UInt<1>
    output io_ptw_status_zero2 : UInt<27>
    output io_ptw_status_sxl : UInt<2>
    output io_ptw_status_uxl : UInt<2>
    output io_ptw_status_sd_rv32 : UInt<1>
    output io_ptw_status_zero1 : UInt<8>
    output io_ptw_status_tsr : UInt<1>
    output io_ptw_status_tw : UInt<1>
    output io_ptw_status_tvm : UInt<1>
    output io_ptw_status_mxr : UInt<1>
    output io_ptw_status_pum : UInt<1>
    output io_ptw_status_mprv : UInt<1>
    output io_ptw_status_xs : UInt<2>
    output io_ptw_status_fs : UInt<2>
    output io_ptw_status_mpp : UInt<2>
    output io_ptw_status_hpp : UInt<2>
    output io_ptw_status_spp : UInt<1>
    output io_ptw_status_mpie : UInt<1>
    output io_ptw_status_hpie : UInt<1>
    output io_ptw_status_spie : UInt<1>
    output io_ptw_status_upie : UInt<1>
    output io_ptw_status_mie : UInt<1>
    output io_ptw_status_hie : UInt<1>
    output io_ptw_status_sie : UInt<1>
    output io_ptw_status_uie : UInt<1>
    output io_fpu_inst : UInt<32>
    output io_fpu_fromint_data : UInt<64>
    output io_fpu_fcsr_rm : UInt<3>
    input io_fpu_fcsr_flags_valid : UInt<1>
    input io_fpu_fcsr_flags_bits : UInt<5>
    input io_fpu_store_data : UInt<64>
    input io_fpu_toint_data : UInt<64>
    output io_fpu_dmem_resp_val : UInt<1>
    output io_fpu_dmem_resp_type : UInt<3>
    output io_fpu_dmem_resp_tag : UInt<5>
    output io_fpu_dmem_resp_data : UInt<64>
    output io_fpu_valid : UInt<1>
    input io_fpu_fcsr_rdy : UInt<1>
    input io_fpu_nack_mem : UInt<1>
    input io_fpu_illegal_rm : UInt<1>
    output io_fpu_killx : UInt<1>
    output io_fpu_killm : UInt<1>
    input io_fpu_dec_cmd : UInt<5>
    input io_fpu_dec_ldst : UInt<1>
    input io_fpu_dec_wen : UInt<1>
    input io_fpu_dec_ren1 : UInt<1>
    input io_fpu_dec_ren2 : UInt<1>
    input io_fpu_dec_ren3 : UInt<1>
    input io_fpu_dec_swap12 : UInt<1>
    input io_fpu_dec_swap23 : UInt<1>
    input io_fpu_dec_single : UInt<1>
    input io_fpu_dec_fromint : UInt<1>
    input io_fpu_dec_toint : UInt<1>
    input io_fpu_dec_fastpipe : UInt<1>
    input io_fpu_dec_fma : UInt<1>
    input io_fpu_dec_div : UInt<1>
    input io_fpu_dec_sqrt : UInt<1>
    input io_fpu_dec_wflags : UInt<1>
    input io_fpu_sboard_set : UInt<1>
    input io_fpu_sboard_clr : UInt<1>
    input io_fpu_sboard_clra : UInt<5>
    input io_rocc_cmd_ready : UInt<1>
    output io_rocc_cmd_valid : UInt<1>
    output io_rocc_cmd_bits_inst_funct : UInt<7>
    output io_rocc_cmd_bits_inst_rs2 : UInt<5>
    output io_rocc_cmd_bits_inst_rs1 : UInt<5>
    output io_rocc_cmd_bits_inst_xd : UInt<1>
    output io_rocc_cmd_bits_inst_xs1 : UInt<1>
    output io_rocc_cmd_bits_inst_xs2 : UInt<1>
    output io_rocc_cmd_bits_inst_rd : UInt<5>
    output io_rocc_cmd_bits_inst_opcode : UInt<7>
    output io_rocc_cmd_bits_rs1 : UInt<64>
    output io_rocc_cmd_bits_rs2 : UInt<64>
    output io_rocc_cmd_bits_status_debug : UInt<1>
    output io_rocc_cmd_bits_status_isa : UInt<32>
    output io_rocc_cmd_bits_status_prv : UInt<2>
    output io_rocc_cmd_bits_status_sd : UInt<1>
    output io_rocc_cmd_bits_status_zero2 : UInt<27>
    output io_rocc_cmd_bits_status_sxl : UInt<2>
    output io_rocc_cmd_bits_status_uxl : UInt<2>
    output io_rocc_cmd_bits_status_sd_rv32 : UInt<1>
    output io_rocc_cmd_bits_status_zero1 : UInt<8>
    output io_rocc_cmd_bits_status_tsr : UInt<1>
    output io_rocc_cmd_bits_status_tw : UInt<1>
    output io_rocc_cmd_bits_status_tvm : UInt<1>
    output io_rocc_cmd_bits_status_mxr : UInt<1>
    output io_rocc_cmd_bits_status_pum : UInt<1>
    output io_rocc_cmd_bits_status_mprv : UInt<1>
    output io_rocc_cmd_bits_status_xs : UInt<2>
    output io_rocc_cmd_bits_status_fs : UInt<2>
    output io_rocc_cmd_bits_status_mpp : UInt<2>
    output io_rocc_cmd_bits_status_hpp : UInt<2>
    output io_rocc_cmd_bits_status_spp : UInt<1>
    output io_rocc_cmd_bits_status_mpie : UInt<1>
    output io_rocc_cmd_bits_status_hpie : UInt<1>
    output io_rocc_cmd_bits_status_spie : UInt<1>
    output io_rocc_cmd_bits_status_upie : UInt<1>
    output io_rocc_cmd_bits_status_mie : UInt<1>
    output io_rocc_cmd_bits_status_hie : UInt<1>
    output io_rocc_cmd_bits_status_sie : UInt<1>
    output io_rocc_cmd_bits_status_uie : UInt<1>
    output io_rocc_resp_ready : UInt<1>
    input io_rocc_resp_valid : UInt<1>
    input io_rocc_resp_bits_rd : UInt<5>
    input io_rocc_resp_bits_data : UInt<64>
    output io_rocc_mem_req_ready : UInt<1>
    input io_rocc_mem_req_valid : UInt<1>
    input io_rocc_mem_req_bits_addr : UInt<40>
    input io_rocc_mem_req_bits_tag : UInt<7>
    input io_rocc_mem_req_bits_cmd : UInt<5>
    input io_rocc_mem_req_bits_typ : UInt<3>
    input io_rocc_mem_req_bits_phys : UInt<1>
    input io_rocc_mem_req_bits_data : UInt<64>
    input io_rocc_mem_s1_kill : UInt<1>
    input io_rocc_mem_s1_data : UInt<64>
    output io_rocc_mem_s2_nack : UInt<1>
    output io_rocc_mem_acquire : UInt<1>
    output io_rocc_mem_release : UInt<1>
    output io_rocc_mem_resp_valid : UInt<1>
    output io_rocc_mem_resp_bits_addr : UInt<40>
    output io_rocc_mem_resp_bits_tag : UInt<7>
    output io_rocc_mem_resp_bits_cmd : UInt<5>
    output io_rocc_mem_resp_bits_typ : UInt<3>
    output io_rocc_mem_resp_bits_data : UInt<64>
    output io_rocc_mem_resp_bits_replay : UInt<1>
    output io_rocc_mem_resp_bits_has_data : UInt<1>
    output io_rocc_mem_resp_bits_data_word_bypass : UInt<64>
    output io_rocc_mem_resp_bits_store_data : UInt<64>
    output io_rocc_mem_replay_next : UInt<1>
    output io_rocc_mem_xcpt_ma_ld : UInt<1>
    output io_rocc_mem_xcpt_ma_st : UInt<1>
    output io_rocc_mem_xcpt_pf_ld : UInt<1>
    output io_rocc_mem_xcpt_pf_st : UInt<1>
    input io_rocc_mem_invalidate_lr : UInt<1>
    output io_rocc_mem_ordered : UInt<1>
    input io_rocc_busy : UInt<1>
    input io_rocc_interrupt : UInt<1>
    output io_rocc_exception : UInt<1>
  
    inst ibuf of IBuf @[Rocket.scala 165:20]
    mem _T_3331 : @[Rocket.scala 682:23]
      data-type => UInt<64>
      depth => 31
      read-latency => 0
      write-latency => 1
      reader => _T_3340
      reader => _T_3350
      writer => _T_4200
      read-under-write => undefined
    inst csr of CSRFile @[Rocket.scala 187:19]
    inst bpu of BreakpointUnit @[Rocket.scala 215:19]
    inst alu of ALU @[Rocket.scala 261:19]
    inst div of MulDiv @[Rocket.scala 268:19]
    skip
    reg ex_ctrl_fp : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_fp) @[Rocket.scala 115:20]
    skip
    reg ex_ctrl_branch : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_branch) @[Rocket.scala 115:20]
    reg ex_ctrl_jal : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_jal) @[Rocket.scala 115:20]
    reg ex_ctrl_jalr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_jalr) @[Rocket.scala 115:20]
    reg ex_ctrl_rxs2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_rxs2) @[Rocket.scala 115:20]
    skip
    reg ex_ctrl_sel_alu2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_alu2) @[Rocket.scala 115:20]
    reg ex_ctrl_sel_alu1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_alu1) @[Rocket.scala 115:20]
    reg ex_ctrl_sel_imm : UInt<3>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_imm) @[Rocket.scala 115:20]
    reg ex_ctrl_alu_dw : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_alu_dw) @[Rocket.scala 115:20]
    reg ex_ctrl_alu_fn : UInt<4>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_alu_fn) @[Rocket.scala 115:20]
    reg ex_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_mem) @[Rocket.scala 115:20]
    reg ex_ctrl_mem_cmd : UInt<5>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_mem_cmd) @[Rocket.scala 115:20]
    reg ex_ctrl_mem_type : UInt<3>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_mem_type) @[Rocket.scala 115:20]
    skip
    skip
    skip
    reg ex_ctrl_wfd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_wfd) @[Rocket.scala 115:20]
    reg ex_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_div) @[Rocket.scala 115:20]
    reg ex_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_wxd) @[Rocket.scala 115:20]
    reg ex_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_csr) @[Rocket.scala 115:20]
    reg ex_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_fence_i) @[Rocket.scala 115:20]
    skip
    skip
    skip
    skip
    reg mem_ctrl_fp : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_fp) @[Rocket.scala 116:21]
    skip
    reg mem_ctrl_branch : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_branch) @[Rocket.scala 116:21]
    reg mem_ctrl_jal : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_jal) @[Rocket.scala 116:21]
    reg mem_ctrl_jalr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_jalr) @[Rocket.scala 116:21]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    reg mem_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_mem) @[Rocket.scala 116:21]
    skip
    skip
    skip
    skip
    skip
    reg mem_ctrl_wfd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_wfd) @[Rocket.scala 116:21]
    reg mem_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_div) @[Rocket.scala 116:21]
    reg mem_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_wxd) @[Rocket.scala 116:21]
    reg mem_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_csr) @[Rocket.scala 116:21]
    reg mem_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_fence_i) @[Rocket.scala 116:21]
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
    reg wb_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_mem) @[Rocket.scala 117:20]
    skip
    skip
    skip
    skip
    skip
    reg wb_ctrl_wfd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_wfd) @[Rocket.scala 117:20]
    reg wb_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_div) @[Rocket.scala 117:20]
    reg wb_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_wxd) @[Rocket.scala 117:20]
    reg wb_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_csr) @[Rocket.scala 117:20]
    reg wb_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_fence_i) @[Rocket.scala 117:20]
    skip
    skip
    skip
    reg ex_reg_xcpt_interrupt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_xcpt_interrupt) @[Rocket.scala 119:35]
    reg ex_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_valid) @[Rocket.scala 120:35]
    reg ex_reg_rvc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rvc) @[Rocket.scala 121:35]
    reg ex_reg_btb_hit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_hit) @[Rocket.scala 122:35]
    reg ex_reg_btb_resp_taken : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_taken) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_mask : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_mask) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_bridx : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_bridx) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_target : UInt<39>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_target) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_entry : UInt<6>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_entry) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_bht_history : UInt<7>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_bht_history) @[Rocket.scala 123:35]
    reg ex_reg_btb_resp_bht_value : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_btb_resp_bht_value) @[Rocket.scala 123:35]
    reg ex_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_xcpt) @[Rocket.scala 124:35]
    reg ex_reg_flush_pipe : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_flush_pipe) @[Rocket.scala 125:35]
    reg ex_reg_load_use : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_load_use) @[Rocket.scala 126:35]
    reg ex_cause : UInt<64>, clock with :
      reset => (UInt<1>("h0"), ex_cause) @[Rocket.scala 127:35]
    reg ex_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_replay) @[Rocket.scala 128:26]
    reg ex_reg_pc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), ex_reg_pc) @[Rocket.scala 129:22]
    reg ex_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), ex_reg_inst) @[Rocket.scala 130:24]
    reg mem_reg_xcpt_interrupt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_xcpt_interrupt) @[Rocket.scala 132:36]
    reg mem_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_valid) @[Rocket.scala 133:36]
    reg mem_reg_rvc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_rvc) @[Rocket.scala 134:36]
    reg mem_reg_btb_hit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_hit) @[Rocket.scala 135:36]
    reg mem_reg_btb_resp_taken : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_taken) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_mask : UInt<2>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_mask) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_bridx : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_bridx) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_target : UInt<39>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_target) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_entry : UInt<6>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_entry) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_bht_history : UInt<7>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_bht_history) @[Rocket.scala 136:36]
    reg mem_reg_btb_resp_bht_value : UInt<2>, clock with :
      reset => (UInt<1>("h0"), mem_reg_btb_resp_bht_value) @[Rocket.scala 136:36]
    reg mem_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_xcpt) @[Rocket.scala 137:36]
    reg mem_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_replay) @[Rocket.scala 138:36]
    reg mem_reg_flush_pipe : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_flush_pipe) @[Rocket.scala 139:36]
    reg mem_reg_cause : UInt<64>, clock with :
      reset => (UInt<1>("h0"), mem_reg_cause) @[Rocket.scala 140:36]
    reg mem_reg_slow_bypass : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_slow_bypass) @[Rocket.scala 141:36]
    reg mem_reg_load : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_load) @[Rocket.scala 142:36]
    reg mem_reg_store : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_store) @[Rocket.scala 143:36]
    reg mem_reg_pc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), mem_reg_pc) @[Rocket.scala 144:23]
    reg mem_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_inst) @[Rocket.scala 145:25]
    reg mem_reg_wdata : UInt<64>, clock with :
      reset => (UInt<1>("h0"), mem_reg_wdata) @[Rocket.scala 146:26]
    reg mem_reg_rs2 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), mem_reg_rs2) @[Rocket.scala 147:24]
    reg wb_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_valid) @[Rocket.scala 150:35]
    reg wb_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_xcpt) @[Rocket.scala 151:35]
    reg wb_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_replay) @[Rocket.scala 152:35]
    reg wb_reg_cause : UInt<64>, clock with :
      reset => (UInt<1>("h0"), wb_reg_cause) @[Rocket.scala 153:35]
    reg wb_reg_pc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), wb_reg_pc) @[Rocket.scala 154:22]
    reg wb_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_inst) @[Rocket.scala 155:24]
    reg wb_reg_wdata : UInt<64>, clock with :
      reset => (UInt<1>("h0"), wb_reg_wdata) @[Rocket.scala 156:25]
    skip
    node replay_wb_common = or(io_dmem_s2_nack, wb_reg_replay) @[Rocket.scala 416:42]
    skip
    skip
    skip
    skip
    node _T_4174 = or(replay_wb_common, wb_reg_xcpt) @[Rocket.scala 420:27]
    node take_pc_wb = or(_T_4174, csr.io_eret) @[Rocket.scala 420:38]
    skip
    node _T_3781 = or(ex_reg_valid, ex_reg_replay) @[Rocket.scala 323:34]
    node ex_pc_valid = or(_T_3781, ex_reg_xcpt_interrupt) @[Rocket.scala 323:51]
    node _T_4001 = shr(mem_reg_wdata, 38) @[Rocket.scala 653:16]
    node _T_4005 = eq(_T_4001, UInt<26>("h0")) @[Rocket.scala 656:13]
    node _T_4007 = eq(_T_4001, UInt<26>("h1")) @[Rocket.scala 656:30]
    node _T_4008 = or(_T_4005, _T_4007) @[Rocket.scala 656:25]
    node _T_4002 = bits(mem_reg_wdata, 39, 38) @[Rocket.scala 654:15]
    node _T_4003 = asSInt(_T_4002) @[Rocket.scala 654:39]
    node _T_4010 = neq(_T_4003, SInt<2>("h0")) @[Rocket.scala 656:45]
    node _T_4011 = asSInt(_T_4001) @[Rocket.scala 657:13]
    node _T_4013 = eq(_T_4011, SInt<26>("h-1")) @[Rocket.scala 657:20]
    skip
    node _T_4016 = eq(_T_4011, SInt<26>("h-2")) @[Rocket.scala 657:45]
    node _T_4017 = or(_T_4013, _T_4016) @[Rocket.scala 657:33]
    node _T_4019 = eq(_T_4003, SInt<2>("h-1")) @[Rocket.scala 657:61]
    node _T_4020 = bits(_T_4003, 0, 0) @[Rocket.scala 657:76]
    node _T_4021 = mux(_T_4017, _T_4019, _T_4020) @[Rocket.scala 657:10]
    node _T_4022 = mux(_T_4008, _T_4010, _T_4021) @[Rocket.scala 656:10]
    node _T_4023 = bits(mem_reg_wdata, 38, 0) @[Rocket.scala 658:16]
    node _T_4024 = cat(_T_4022, _T_4023) @[Cat.scala 30:58]
    node _T_4025 = asSInt(_T_4024) @[Rocket.scala 343:88]
    node _T_3820 = asSInt(mem_reg_pc) @[Rocket.scala 339:34]
    node mem_br_taken = bits(mem_reg_wdata, 0, 0) @[Rocket.scala 338:35]
    node _T_3821 = and(mem_ctrl_branch, mem_br_taken) @[Rocket.scala 340:25]
    skip
    node _T_3826 = bits(mem_reg_inst, 31, 31) @[Rocket.scala 704:48]
    node _T_3827 = asSInt(_T_3826) @[Rocket.scala 704:53]
    skip
    node _T_3902 = asUInt(_T_3827) @[Cat.scala 30:58]
    skip
    skip
    skip
    node _T_3833 = pad(_T_3827, 11) @[Rocket.scala 705:21]
    node _T_3901 = asUInt(_T_3833) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    node _T_3839 = bits(mem_reg_inst, 19, 12) @[Rocket.scala 706:65]
    node _T_3840 = asSInt(_T_3839) @[Rocket.scala 706:73]
    node _T_3841 = pad(_T_3827, 8) @[Rocket.scala 706:21]
    node _T_3899 = asUInt(_T_3841) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    node _T_3850 = bits(mem_reg_inst, 20, 20) @[Rocket.scala 708:39]
    node _T_3851 = asSInt(_T_3850) @[Rocket.scala 708:44]
    skip
    node _T_3854 = bits(mem_reg_inst, 7, 7) @[Rocket.scala 709:39]
    node _T_3855 = asSInt(_T_3854) @[Rocket.scala 709:43]
    skip
    skip
    skip
    node _T_3898 = asUInt(_T_3855) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    node _T_3865 = bits(mem_reg_inst, 30, 25) @[Rocket.scala 710:66]
    skip
    skip
    skip
    skip
    skip
    node _T_3875 = bits(mem_reg_inst, 11, 8) @[Rocket.scala 712:57]
    skip
    skip
    node _T_3879 = bits(mem_reg_inst, 24, 21) @[Rocket.scala 713:52]
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
    node _T_3905 = cat(cat(cat(_T_3902, _T_3901), cat(_T_3899, _T_3898)), cat(cat(_T_3865, _T_3875), UInt<1>("h0"))) @[Cat.scala 30:58]
    node _T_3906 = asSInt(_T_3905) @[Rocket.scala 718:53]
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
    node _T_3986 = asUInt(_T_3840) @[Cat.scala 30:58]
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
    node _T_3985 = asUInt(_T_3851) @[Cat.scala 30:58]
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
    node _T_3992 = cat(cat(cat(_T_3902, _T_3901), cat(_T_3986, _T_3985)), cat(cat(_T_3865, _T_3879), UInt<1>("h0"))) @[Cat.scala 30:58]
    node _T_3993 = asSInt(_T_3992) @[Rocket.scala 718:53]
    node _T_3996 = mux(mem_reg_rvc, SInt<4>("h2"), SInt<4>("h4")) @[Rocket.scala 342:8]
    node _T_3997 = mux(mem_ctrl_jal, _T_3993, pad(_T_3996, 32)) @[Rocket.scala 341:8]
    node _T_3998 = mux(_T_3821, _T_3906, _T_3997) @[Rocket.scala 340:8]
    node _GEN_185 = pad(_T_3998, 40) @[Rocket.scala 339:41]
    node _T_3999 = add(_T_3820, _GEN_185) @[Rocket.scala 339:41]
    node _T_4000 = tail(_T_3999, 1) @[Rocket.scala 339:41]
    node mem_br_target = asSInt(_T_4000) @[Rocket.scala 339:41]
    node _T_4026 = mux(mem_ctrl_jalr, _T_4025, mem_br_target) @[Rocket.scala 343:21]
    node _T_4028 = and(_T_4026, SInt<40>("h-2")) @[Rocket.scala 343:111]
    node _T_4029 = asSInt(_T_4028) @[Rocket.scala 343:111]
    node mem_npc = asUInt(_T_4029) @[Rocket.scala 343:123]
    node _T_4030 = neq(mem_npc, ex_reg_pc) @[Rocket.scala 344:48]
    node _T_4031 = neq(mem_npc, ibuf.io_pc) @[Rocket.scala 344:98]
    node _T_4033 = mux(ibuf.io_inst_0_valid, _T_4031, UInt<1>("h1")) @[Rocket.scala 344:66]
    node mem_misprediction = mux(ex_pc_valid, _T_4030, _T_4033) @[Rocket.scala 344:26]
    node _T_4051 = or(mem_misprediction, mem_reg_flush_pipe) @[Rocket.scala 351:54]
    node take_pc_mem = and(mem_reg_valid, _T_4051) @[Rocket.scala 351:32]
    skip
    node take_pc_mem_wb = or(take_pc_wb, take_pc_mem) @[Rocket.scala 161:35]
    node _T_4425 = eq(ibuf.io_inst_0_valid, UInt<1>("h0")) @[Rocket.scala 535:17]
    node _T_4426 = or(_T_4425, ibuf.io_inst_0_bits_replay) @[Rocket.scala 535:40]
    node _T_4427 = or(_T_4426, take_pc_mem_wb) @[Rocket.scala 535:71]
    node _T_2856 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h44")) @[Decode.scala 13:65]
    node _T_2858 = eq(_T_2856, UInt<32>("h0")) @[Decode.scala 13:121]
    skip
    node _T_2860 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4024")) @[Decode.scala 13:65]
    node _T_2862 = eq(_T_2860, UInt<32>("h20")) @[Decode.scala 13:121]
    node _T_2877 = or(_T_2858, _T_2862) @[Decode.scala 14:30]
    node _T_2864 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h38")) @[Decode.scala 13:65]
    node _T_2866 = eq(_T_2864, UInt<32>("h20")) @[Decode.scala 13:121]
    node _T_2878 = or(_T_2877, _T_2866) @[Decode.scala 14:30]
    node _T_2868 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2050")) @[Decode.scala 13:65]
    node _T_2870 = eq(_T_2868, UInt<32>("h2000")) @[Decode.scala 13:121]
    node _T_2879 = or(_T_2878, _T_2870) @[Decode.scala 14:30]
    node _T_2872 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h90000034")) @[Decode.scala 13:65]
    node _T_2874 = eq(_T_2872, UInt<32>("h90000010")) @[Decode.scala 13:121]
    node id_ctrl_rxs1 = or(_T_2879, _T_2874) @[Decode.scala 14:30]
    skip
    node _T_4232 = neq(ibuf.io_inst_0_bits_inst_rs1, UInt<5>("h0")) @[Rocket.scala 479:55]
    node _T_4233 = and(id_ctrl_rxs1, _T_4232) @[Rocket.scala 479:42]
    node ex_waddr = bits(ex_reg_inst, 11, 7) @[Rocket.scala 235:29]
    node _T_4275 = eq(ibuf.io_inst_0_bits_inst_rs1, ex_waddr) @[Rocket.scala 494:70]
    node _T_4276 = and(_T_4233, _T_4275) @[Rocket.scala 648:27]
    node _T_2835 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h64")) @[Decode.scala 13:65]
    node _T_2837 = eq(_T_2835, UInt<32>("h20")) @[Decode.scala 13:121]
    skip
    node _T_2839 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h34")) @[Decode.scala 13:65]
    node _T_2841 = eq(_T_2839, UInt<32>("h20")) @[Decode.scala 13:121]
    node _T_2852 = or(_T_2837, _T_2841) @[Decode.scala 14:30]
    node _T_2843 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2048")) @[Decode.scala 13:65]
    node _T_2845 = eq(_T_2843, UInt<32>("h2008")) @[Decode.scala 13:121]
    node _T_2853 = or(_T_2852, _T_2845) @[Decode.scala 14:30]
    node _T_2847 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h42003024")) @[Decode.scala 13:65]
    node _T_2849 = eq(_T_2847, UInt<32>("h2000020")) @[Decode.scala 13:121]
    node id_ctrl_rxs2 = or(_T_2853, _T_2849) @[Decode.scala 14:30]
    skip
    node _T_4235 = neq(ibuf.io_inst_0_bits_inst_rs2, UInt<5>("h0")) @[Rocket.scala 480:55]
    node _T_4236 = and(id_ctrl_rxs2, _T_4235) @[Rocket.scala 480:42]
    node _T_4277 = eq(ibuf.io_inst_0_bits_inst_rs2, ex_waddr) @[Rocket.scala 494:70]
    node _T_4278 = and(_T_4236, _T_4277) @[Rocket.scala 648:27]
    node _T_4281 = or(_T_4276, _T_4278) @[Rocket.scala 648:50]
    skip
    node _T_3240 = eq(_T_2835, UInt<32>("h0")) @[Decode.scala 13:121]
    skip
    node _T_3242 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h50")) @[Decode.scala 13:65]
    node _T_3244 = eq(_T_3242, UInt<32>("h10")) @[Decode.scala 13:121]
    node _T_3267 = or(_T_3240, _T_3244) @[Decode.scala 14:30]
    node _T_3246 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2024")) @[Decode.scala 13:65]
    node _T_3248 = eq(_T_3246, UInt<32>("h24")) @[Decode.scala 13:121]
    node _T_3268 = or(_T_3267, _T_3248) @[Decode.scala 14:30]
    node _T_3250 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h28")) @[Decode.scala 13:65]
    node _T_3252 = eq(_T_3250, UInt<32>("h28")) @[Decode.scala 13:121]
    node _T_3269 = or(_T_3268, _T_3252) @[Decode.scala 14:30]
    node _T_3254 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1030")) @[Decode.scala 13:65]
    node _T_3256 = eq(_T_3254, UInt<32>("h1030")) @[Decode.scala 13:121]
    node _T_3270 = or(_T_3269, _T_3256) @[Decode.scala 14:30]
    node _T_3258 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2030")) @[Decode.scala 13:65]
    node _T_3260 = eq(_T_3258, UInt<32>("h2030")) @[Decode.scala 13:121]
    node _T_3271 = or(_T_3270, _T_3260) @[Decode.scala 14:30]
    node _T_3262 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h90000010")) @[Decode.scala 13:65]
    node _T_3264 = eq(_T_3262, UInt<32>("h80000010")) @[Decode.scala 13:121]
    node id_ctrl_wxd = or(_T_3271, _T_3264) @[Decode.scala 14:30]
    skip
    node _T_4238 = neq(ibuf.io_inst_0_bits_inst_rd, UInt<5>("h0")) @[Rocket.scala 481:55]
    node _T_4239 = and(id_ctrl_wxd, _T_4238) @[Rocket.scala 481:42]
    node _T_4279 = eq(ibuf.io_inst_0_bits_inst_rd, ex_waddr) @[Rocket.scala 494:70]
    node _T_4280 = and(_T_4239, _T_4279) @[Rocket.scala 648:27]
    node _T_4282 = or(_T_4281, _T_4280) @[Rocket.scala 648:50]
    node data_hazard_ex = and(ex_ctrl_wxd, _T_4282) @[Rocket.scala 494:36]
    node _T_4270 = neq(ex_ctrl_csr, UInt<3>("h0")) @[Rocket.scala 493:38]
    node _T_4271 = or(_T_4270, ex_ctrl_jalr) @[Rocket.scala 493:48]
    node _T_4272 = or(_T_4271, ex_ctrl_mem) @[Rocket.scala 493:64]
    node _T_4273 = or(_T_4272, ex_ctrl_div) @[Rocket.scala 493:79]
    node ex_cannot_bypass = or(_T_4273, ex_ctrl_fp) @[Rocket.scala 493:94]
    skip
    node _T_4294 = and(data_hazard_ex, ex_cannot_bypass) @[Rocket.scala 496:54]
    skip
    node _T_4284 = and(io_fpu_dec_ren1, _T_4275) @[Rocket.scala 648:27]
    skip
    node _T_4286 = and(io_fpu_dec_ren2, _T_4277) @[Rocket.scala 648:27]
    node _T_4291 = or(_T_4284, _T_4286) @[Rocket.scala 648:50]
    node _T_4287 = eq(ibuf.io_inst_0_bits_inst_rs3, ex_waddr) @[Rocket.scala 495:76]
    node _T_4288 = and(io_fpu_dec_ren3, _T_4287) @[Rocket.scala 648:27]
    node _T_4292 = or(_T_4291, _T_4288) @[Rocket.scala 648:50]
    skip
    node _T_4290 = and(io_fpu_dec_wen, _T_4279) @[Rocket.scala 648:27]
    node _T_4293 = or(_T_4292, _T_4290) @[Rocket.scala 648:50]
    node fp_data_hazard_ex = and(ex_ctrl_wfd, _T_4293) @[Rocket.scala 495:39]
    node _T_4295 = or(_T_4294, fp_data_hazard_ex) @[Rocket.scala 496:74]
    node id_ex_hazard = and(ex_reg_valid, _T_4295) @[Rocket.scala 496:35]
    node mem_waddr = bits(mem_reg_inst, 11, 7) @[Rocket.scala 236:31]
    node _T_4303 = eq(ibuf.io_inst_0_bits_inst_rs1, mem_waddr) @[Rocket.scala 503:72]
    node _T_4304 = and(_T_4233, _T_4303) @[Rocket.scala 648:27]
    node _T_4305 = eq(ibuf.io_inst_0_bits_inst_rs2, mem_waddr) @[Rocket.scala 503:72]
    node _T_4306 = and(_T_4236, _T_4305) @[Rocket.scala 648:27]
    node _T_4309 = or(_T_4304, _T_4306) @[Rocket.scala 648:50]
    node _T_4307 = eq(ibuf.io_inst_0_bits_inst_rd, mem_waddr) @[Rocket.scala 503:72]
    node _T_4308 = and(_T_4239, _T_4307) @[Rocket.scala 648:27]
    node _T_4310 = or(_T_4309, _T_4308) @[Rocket.scala 648:50]
    node data_hazard_mem = and(mem_ctrl_wxd, _T_4310) @[Rocket.scala 503:38]
    node _T_4298 = neq(mem_ctrl_csr, UInt<3>("h0")) @[Rocket.scala 502:40]
    skip
    node _T_4299 = and(mem_ctrl_mem, mem_reg_slow_bypass) @[Rocket.scala 502:66]
    node _T_4300 = or(_T_4298, _T_4299) @[Rocket.scala 502:50]
    node _T_4301 = or(_T_4300, mem_ctrl_div) @[Rocket.scala 502:84]
    node mem_cannot_bypass = or(_T_4301, mem_ctrl_fp) @[Rocket.scala 502:100]
    skip
    node _T_4322 = and(data_hazard_mem, mem_cannot_bypass) @[Rocket.scala 505:57]
    skip
    node _T_4312 = and(io_fpu_dec_ren1, _T_4303) @[Rocket.scala 648:27]
    skip
    node _T_4314 = and(io_fpu_dec_ren2, _T_4305) @[Rocket.scala 648:27]
    node _T_4319 = or(_T_4312, _T_4314) @[Rocket.scala 648:50]
    node _T_4315 = eq(ibuf.io_inst_0_bits_inst_rs3, mem_waddr) @[Rocket.scala 504:78]
    node _T_4316 = and(io_fpu_dec_ren3, _T_4315) @[Rocket.scala 648:27]
    node _T_4320 = or(_T_4319, _T_4316) @[Rocket.scala 648:50]
    skip
    node _T_4318 = and(io_fpu_dec_wen, _T_4307) @[Rocket.scala 648:27]
    node _T_4321 = or(_T_4320, _T_4318) @[Rocket.scala 648:50]
    node fp_data_hazard_mem = and(mem_ctrl_wfd, _T_4321) @[Rocket.scala 504:41]
    node _T_4323 = or(_T_4322, fp_data_hazard_mem) @[Rocket.scala 505:78]
    node id_mem_hazard = and(mem_reg_valid, _T_4323) @[Rocket.scala 505:37]
    node _T_4405 = or(id_ex_hazard, id_mem_hazard) @[Rocket.scala 528:18]
    node wb_waddr = bits(wb_reg_inst, 11, 7) @[Rocket.scala 237:29]
    node _T_4326 = eq(ibuf.io_inst_0_bits_inst_rs1, wb_waddr) @[Rocket.scala 509:70]
    node _T_4327 = and(_T_4233, _T_4326) @[Rocket.scala 648:27]
    node _T_4328 = eq(ibuf.io_inst_0_bits_inst_rs2, wb_waddr) @[Rocket.scala 509:70]
    node _T_4329 = and(_T_4236, _T_4328) @[Rocket.scala 648:27]
    node _T_4332 = or(_T_4327, _T_4329) @[Rocket.scala 648:50]
    node _T_4330 = eq(ibuf.io_inst_0_bits_inst_rd, wb_waddr) @[Rocket.scala 509:70]
    node _T_4331 = and(_T_4239, _T_4330) @[Rocket.scala 648:27]
    node _T_4333 = or(_T_4332, _T_4331) @[Rocket.scala 648:50]
    node data_hazard_wb = and(wb_ctrl_wxd, _T_4333) @[Rocket.scala 509:36]
    node _T_3783 = eq(io_dmem_resp_valid, UInt<1>("h0")) @[Rocket.scala 324:39]
    node wb_dcache_miss = and(wb_ctrl_mem, _T_3783) @[Rocket.scala 324:36]
    node wb_set_sboard = or(wb_ctrl_div, wb_dcache_miss) @[Rocket.scala 415:35]
    skip
    node _T_4345 = and(data_hazard_wb, wb_set_sboard) @[Rocket.scala 511:54]
    skip
    node _T_4335 = and(io_fpu_dec_ren1, _T_4326) @[Rocket.scala 648:27]
    skip
    node _T_4337 = and(io_fpu_dec_ren2, _T_4328) @[Rocket.scala 648:27]
    node _T_4342 = or(_T_4335, _T_4337) @[Rocket.scala 648:50]
    node _T_4338 = eq(ibuf.io_inst_0_bits_inst_rs3, wb_waddr) @[Rocket.scala 510:76]
    node _T_4339 = and(io_fpu_dec_ren3, _T_4338) @[Rocket.scala 648:27]
    node _T_4343 = or(_T_4342, _T_4339) @[Rocket.scala 648:50]
    skip
    node _T_4341 = and(io_fpu_dec_wen, _T_4330) @[Rocket.scala 648:27]
    node _T_4344 = or(_T_4343, _T_4341) @[Rocket.scala 648:50]
    node fp_data_hazard_wb = and(wb_ctrl_wfd, _T_4344) @[Rocket.scala 510:39]
    node _T_4346 = or(_T_4345, fp_data_hazard_wb) @[Rocket.scala 511:71]
    node id_wb_hazard = and(wb_reg_valid, _T_4346) @[Rocket.scala 511:35]
    node _T_4406 = or(_T_4405, id_wb_hazard) @[Rocket.scala 528:35]
    reg _T_4241 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_4241) @[Rocket.scala 668:25]
    node _T_4242 = shr(_T_4241, 1) @[Rocket.scala 669:35]
    node _T_4243 = shl(_T_4242, 1) @[Rocket.scala 669:40]
    node _T_4252 = dshr(_T_4243, ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 665:35]
    node _T_4253 = bits(_T_4252, 0, 0) @[Rocket.scala 665:35]
    node _T_4254 = and(_T_4233, _T_4253) @[Rocket.scala 648:27]
    node _T_4255 = dshr(_T_4243, ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 665:35]
    node _T_4256 = bits(_T_4255, 0, 0) @[Rocket.scala 665:35]
    node _T_4257 = and(_T_4236, _T_4256) @[Rocket.scala 648:27]
    node _T_4261 = or(_T_4254, _T_4257) @[Rocket.scala 648:50]
    node _T_4258 = dshr(_T_4243, ibuf.io_inst_0_bits_inst_rd) @[Rocket.scala 665:35]
    node _T_4259 = bits(_T_4258, 0, 0) @[Rocket.scala 665:35]
    node _T_4260 = and(_T_4239, _T_4259) @[Rocket.scala 648:27]
    node id_sboard_hazard = or(_T_4261, _T_4260) @[Rocket.scala 648:50]
    node _T_4407 = or(_T_4406, id_sboard_hazard) @[Rocket.scala 528:51]
    node _T_2805 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h5c")) @[Decode.scala 13:65]
    node _T_2807 = eq(_T_2805, UInt<32>("h4")) @[Decode.scala 13:121]
    skip
    node _T_2809 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h60")) @[Decode.scala 13:65]
    node _T_2811 = eq(_T_2809, UInt<32>("h40")) @[Decode.scala 13:121]
    node id_ctrl_fp = or(_T_2807, _T_2811) @[Decode.scala 14:30]
    skip
    node _T_3286 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3070")) @[Decode.scala 13:65]
    node _T_3288 = eq(_T_3286, UInt<32>("h70")) @[Decode.scala 13:121]
    skip
    node _T_3280 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2070")) @[Decode.scala 13:65]
    node _T_3282 = eq(_T_3280, UInt<32>("h2070")) @[Decode.scala 13:121]
    skip
    skip
    node _T_3274 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1070")) @[Decode.scala 13:65]
    node _T_3276 = eq(_T_3274, UInt<32>("h1070")) @[Decode.scala 13:121]
    skip
    node id_ctrl_csr = cat(cat(_T_3288, _T_3282), _T_3276) @[Cat.scala 30:58]
    skip
    node _T_3450 = eq(id_ctrl_csr, UInt<3>("h2")) @[Package.scala 7:47]
    node _T_3451 = eq(id_ctrl_csr, UInt<3>("h3")) @[Package.scala 7:47]
    node _T_3453 = or(_T_3450, _T_3451) @[Package.scala 7:62]
    node _T_3452 = eq(id_ctrl_csr, UInt<3>("h1")) @[Package.scala 7:47]
    node id_csr_en = or(_T_3453, _T_3452) @[Package.scala 7:62]
    node _T_4375 = eq(io_fpu_fcsr_rdy, UInt<1>("h0")) @[Rocket.scala 519:18]
    node _T_4376 = and(id_csr_en, _T_4375) @[Rocket.scala 519:15]
    reg _T_4348 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_4348) @[Rocket.scala 668:25]
    node _T_4377 = dshr(_T_4348, ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 665:35]
    node _T_4378 = bits(_T_4377, 0, 0) @[Rocket.scala 665:35]
    node _T_4379 = and(io_fpu_dec_ren1, _T_4378) @[Rocket.scala 648:27]
    node _T_4380 = dshr(_T_4348, ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 665:35]
    node _T_4381 = bits(_T_4380, 0, 0) @[Rocket.scala 665:35]
    node _T_4382 = and(io_fpu_dec_ren2, _T_4381) @[Rocket.scala 648:27]
    node _T_4389 = or(_T_4379, _T_4382) @[Rocket.scala 648:50]
    node _T_4383 = dshr(_T_4348, ibuf.io_inst_0_bits_inst_rs3) @[Rocket.scala 665:35]
    node _T_4384 = bits(_T_4383, 0, 0) @[Rocket.scala 665:35]
    node _T_4385 = and(io_fpu_dec_ren3, _T_4384) @[Rocket.scala 648:27]
    node _T_4390 = or(_T_4389, _T_4385) @[Rocket.scala 648:50]
    node _T_4386 = dshr(_T_4348, ibuf.io_inst_0_bits_inst_rd) @[Rocket.scala 665:35]
    node _T_4387 = bits(_T_4386, 0, 0) @[Rocket.scala 665:35]
    node _T_4388 = and(io_fpu_dec_wen, _T_4387) @[Rocket.scala 648:27]
    node _T_4391 = or(_T_4390, _T_4388) @[Rocket.scala 648:50]
    node id_stall_fpu = or(_T_4376, _T_4391) @[Rocket.scala 519:35]
    node _T_4408 = and(id_ctrl_fp, id_stall_fpu) @[Rocket.scala 529:16]
    node _T_4409 = or(_T_4407, _T_4408) @[Rocket.scala 528:71]
    node _T_3083 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h405f")) @[Decode.scala 13:65]
    node _T_3085 = eq(_T_3083, UInt<32>("h3")) @[Decode.scala 13:121]
    skip
    node _T_2603 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h207f")) @[Decode.scala 13:65]
    node _T_2605 = eq(_T_2603, UInt<32>("h3")) @[Decode.scala 13:121]
    node _T_3096 = or(_T_3085, _T_2605) @[Decode.scala 14:30]
    node _T_3087 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h107f")) @[Decode.scala 13:65]
    node _T_3089 = eq(_T_3087, UInt<32>("h3")) @[Decode.scala 13:121]
    node _T_3097 = or(_T_3096, _T_3089) @[Decode.scala 14:30]
    node _T_3091 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h707f")) @[Decode.scala 13:65]
    node _T_3093 = eq(_T_3091, UInt<32>("h100f")) @[Decode.scala 13:121]
    node _T_3098 = or(_T_3097, _T_3093) @[Decode.scala 14:30]
    node _T_2659 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h605b")) @[Decode.scala 13:65]
    node _T_2661 = eq(_T_2659, UInt<32>("h2003")) @[Decode.scala 13:121]
    node _T_3099 = or(_T_3098, _T_2661) @[Decode.scala 14:30]
    node _T_2667 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1800607f")) @[Decode.scala 13:65]
    node _T_2669 = eq(_T_2667, UInt<32>("h202f")) @[Decode.scala 13:121]
    node _T_3100 = or(_T_3099, _T_2669) @[Decode.scala 14:30]
    node _T_2691 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("he800607f")) @[Decode.scala 13:65]
    node _T_2693 = eq(_T_2691, UInt<32>("h800202f")) @[Decode.scala 13:121]
    node _T_3101 = or(_T_3100, _T_2693) @[Decode.scala 14:30]
    node _T_2695 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hf9f0607f")) @[Decode.scala 13:65]
    node _T_2697 = eq(_T_2695, UInt<32>("h1000202f")) @[Decode.scala 13:121]
    node id_ctrl_mem = or(_T_3101, _T_2697) @[Decode.scala 14:30]
    skip
    reg dcache_blocked : UInt<1>, clock with :
      reset => (UInt<1>("h0"), dcache_blocked) @[Rocket.scala 522:27]
    node _T_4410 = and(id_ctrl_mem, dcache_blocked) @[Rocket.scala 530:17]
    node _T_4411 = or(_T_4409, _T_4410) @[Rocket.scala 529:32]
    skip
    skip
    skip
    skip
    node _T_3232 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2000074")) @[Decode.scala 13:65]
    node id_ctrl_div = eq(_T_3232, UInt<32>("h2000030")) @[Decode.scala 13:121]
    skip
    skip
    node wb_wxd = and(wb_reg_valid, wb_ctrl_wxd) @[Rocket.scala 414:29]
    node _T_4415 = eq(wb_wxd, UInt<1>("h0")) @[Rocket.scala 532:65]
    node _T_4416 = and(div.io_resp_valid, _T_4415) @[Rocket.scala 532:62]
    node _T_4417 = or(div.io_req_ready, _T_4416) @[Rocket.scala 532:40]
    node _T_4419 = eq(_T_4417, UInt<1>("h0")) @[Rocket.scala 532:21]
    node _T_4420 = or(_T_4419, div.io_req_valid) @[Rocket.scala 532:75]
    node _T_4421 = and(id_ctrl_div, _T_4420) @[Rocket.scala 532:17]
    node _T_4422 = or(_T_4411, _T_4421) @[Rocket.scala 531:34]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_3300 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3058")) @[Decode.scala 13:65]
    node id_ctrl_fence = eq(_T_3300, UInt<32>("h8")) @[Decode.scala 13:121]
    skip
    skip
    skip
    node _T_3503 = eq(io_dmem_ordered, UInt<1>("h0")) @[Rocket.scala 207:21]
    node id_mem_busy = or(_T_3503, io_dmem_req_valid) @[Rocket.scala 207:38]
    node _T_3306 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h6048")) @[Decode.scala 13:65]
    node id_ctrl_amo = eq(_T_3306, UInt<32>("h2008")) @[Decode.scala 13:121]
    skip
    skip
    node id_amo_aq = bits(ibuf.io_inst_0_bits_inst_bits, 26, 26) @[Rocket.scala 204:29]
    node _T_3514 = and(id_ctrl_amo, id_amo_aq) @[Rocket.scala 213:33]
    skip
    node id_ctrl_fence_i = eq(_T_3300, UInt<32>("h1008")) @[Decode.scala 13:121]
    skip
    skip
    node _T_3515 = or(_T_3514, id_ctrl_fence_i) @[Rocket.scala 213:46]
    reg id_reg_fence : UInt<1>, clock with :
      reset => (UInt<1>("h0"), id_reg_fence) @[Rocket.scala 178:25]
    skip
    node _T_3517 = and(id_reg_fence, id_ctrl_mem) @[Rocket.scala 213:81]
    node _T_3518 = or(_T_3515, _T_3517) @[Rocket.scala 213:65]
    node id_do_fence = and(id_mem_busy, _T_3518) @[Rocket.scala 213:17]
    skip
    node _T_4423 = or(_T_4422, id_do_fence) @[Rocket.scala 532:96]
    node ctrl_stalld = or(_T_4423, csr.io_csr_stall) @[Rocket.scala 533:17]
    node _T_4428 = or(_T_4427, ctrl_stalld) @[Rocket.scala 535:89]
    node ctrl_killd = or(_T_4428, csr.io_interrupt) @[Rocket.scala 535:104]
    skip
    node _T_3444 = eq(ctrl_killd, UInt<1>("h0")) @[Rocket.scala 185:34]
    skip
    node _T_2823 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h68")) @[Decode.scala 13:65]
    node id_ctrl_jal = eq(_T_2823, UInt<32>("h68")) @[Decode.scala 13:121]
    skip
    skip
    skip
    skip
    skip
    node _T_2607 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h106f")) @[Decode.scala 13:65]
    node _T_2609 = eq(_T_2607, UInt<32>("h3")) @[Decode.scala 13:121]
    node _T_2611 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h607f")) @[Decode.scala 13:65]
    node _T_2613 = eq(_T_2611, UInt<32>("hf")) @[Decode.scala 13:121]
    node _T_2615 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7077")) @[Decode.scala 13:65]
    node _T_2617 = eq(_T_2615, UInt<32>("h13")) @[Decode.scala 13:121]
    node _T_2619 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h5f")) @[Decode.scala 13:65]
    node _T_2621 = eq(_T_2619, UInt<32>("h17")) @[Decode.scala 13:121]
    node _T_2623 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfc00007f")) @[Decode.scala 13:65]
    node _T_2625 = eq(_T_2623, UInt<32>("h33")) @[Decode.scala 13:121]
    node _T_2627 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hbe007077")) @[Decode.scala 13:65]
    node _T_2629 = eq(_T_2627, UInt<32>("h33")) @[Decode.scala 13:121]
    node _T_2631 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4000073")) @[Decode.scala 13:65]
    node _T_2633 = eq(_T_2631, UInt<32>("h43")) @[Decode.scala 13:121]
    node _T_2635 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("he400007f")) @[Decode.scala 13:65]
    node _T_2637 = eq(_T_2635, UInt<32>("h53")) @[Decode.scala 13:121]
    node _T_2639 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h707b")) @[Decode.scala 13:65]
    node _T_2641 = eq(_T_2639, UInt<32>("h63")) @[Decode.scala 13:121]
    node _T_2643 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7f")) @[Decode.scala 13:65]
    node _T_2645 = eq(_T_2643, UInt<32>("h6f")) @[Decode.scala 13:121]
    node _T_2647 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hffefffff")) @[Decode.scala 13:65]
    node _T_2649 = eq(_T_2647, UInt<32>("h73")) @[Decode.scala 13:121]
    node _T_2651 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfc00305f")) @[Decode.scala 13:65]
    node _T_2653 = eq(_T_2651, UInt<32>("h1013")) @[Decode.scala 13:121]
    node _T_2655 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfe00305f")) @[Decode.scala 13:65]
    node _T_2657 = eq(_T_2655, UInt<32>("h101b")) @[Decode.scala 13:121]
    skip
    node _T_2665 = eq(_T_2603, UInt<32>("h2013")) @[Decode.scala 13:121]
    skip
    node _T_2673 = eq(_T_2603, UInt<32>("h2073")) @[Decode.scala 13:121]
    node _T_2675 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hbc00707f")) @[Decode.scala 13:65]
    node _T_2677 = eq(_T_2675, UInt<32>("h5013")) @[Decode.scala 13:121]
    node _T_2679 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hbe00705f")) @[Decode.scala 13:65]
    node _T_2681 = eq(_T_2679, UInt<32>("h501b")) @[Decode.scala 13:121]
    skip
    node _T_2685 = eq(_T_2627, UInt<32>("h5033")) @[Decode.scala 13:121]
    node _T_2687 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfe004077")) @[Decode.scala 13:65]
    node _T_2689 = eq(_T_2687, UInt<32>("h2004033")) @[Decode.scala 13:121]
    node _T_2699 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hdfffffff")) @[Decode.scala 13:65]
    node _T_2701 = eq(_T_2699, UInt<32>("h10200073")) @[Decode.scala 13:121]
    node _T_2703 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10500073")) @[Decode.scala 13:121]
    node _T_2705 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfe007fff")) @[Decode.scala 13:65]
    node _T_2707 = eq(_T_2705, UInt<32>("h12000073")) @[Decode.scala 13:121]
    node _T_2709 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hf400607f")) @[Decode.scala 13:65]
    node _T_2711 = eq(_T_2709, UInt<32>("h20000053")) @[Decode.scala 13:121]
    node _T_2713 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7c00607f")) @[Decode.scala 13:65]
    node _T_2715 = eq(_T_2713, UInt<32>("h20000053")) @[Decode.scala 13:121]
    node _T_2717 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7c00507f")) @[Decode.scala 13:65]
    node _T_2719 = eq(_T_2717, UInt<32>("h20000053")) @[Decode.scala 13:121]
    node _T_2721 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7ff0007f")) @[Decode.scala 13:65]
    node _T_2723 = eq(_T_2721, UInt<32>("h40100053")) @[Decode.scala 13:121]
    skip
    node _T_2727 = eq(_T_2721, UInt<32>("h42000053")) @[Decode.scala 13:121]
    node _T_2729 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfdf0007f")) @[Decode.scala 13:65]
    node _T_2731 = eq(_T_2729, UInt<32>("h58000053")) @[Decode.scala 13:121]
    node _T_2733 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7b200073")) @[Decode.scala 13:121]
    node _T_2735 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hedc0007f")) @[Decode.scala 13:65]
    node _T_2737 = eq(_T_2735, UInt<32>("hc0000053")) @[Decode.scala 13:121]
    node _T_2739 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfdf0607f")) @[Decode.scala 13:65]
    node _T_2741 = eq(_T_2739, UInt<32>("he0000053")) @[Decode.scala 13:121]
    node _T_2743 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hedf0707f")) @[Decode.scala 13:65]
    node _T_2745 = eq(_T_2743, UInt<32>("he0000053")) @[Decode.scala 13:121]
    node _T_2747 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h603f")) @[Decode.scala 13:65]
    node _T_2749 = eq(_T_2747, UInt<32>("h23")) @[Decode.scala 13:121]
    node _T_2751 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h306f")) @[Decode.scala 13:65]
    node _T_2753 = eq(_T_2751, UInt<32>("h1063")) @[Decode.scala 13:121]
    node _T_2755 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h407f")) @[Decode.scala 13:65]
    node _T_2757 = eq(_T_2755, UInt<32>("h4063")) @[Decode.scala 13:121]
    node _T_2759 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfc007077")) @[Decode.scala 13:65]
    node _T_2761 = eq(_T_2759, UInt<32>("h33")) @[Decode.scala 13:121]
    skip
    node _T_2764 = or(_T_2605, _T_2609) @[Decode.scala 14:30]
    node _T_2765 = or(_T_2764, _T_2613) @[Decode.scala 14:30]
    node _T_2766 = or(_T_2765, _T_2617) @[Decode.scala 14:30]
    node _T_2767 = or(_T_2766, _T_2621) @[Decode.scala 14:30]
    node _T_2768 = or(_T_2767, _T_2625) @[Decode.scala 14:30]
    node _T_2769 = or(_T_2768, _T_2629) @[Decode.scala 14:30]
    node _T_2770 = or(_T_2769, _T_2633) @[Decode.scala 14:30]
    node _T_2771 = or(_T_2770, _T_2637) @[Decode.scala 14:30]
    node _T_2772 = or(_T_2771, _T_2641) @[Decode.scala 14:30]
    node _T_2773 = or(_T_2772, _T_2645) @[Decode.scala 14:30]
    node _T_2774 = or(_T_2773, _T_2649) @[Decode.scala 14:30]
    node _T_2775 = or(_T_2774, _T_2653) @[Decode.scala 14:30]
    node _T_2776 = or(_T_2775, _T_2657) @[Decode.scala 14:30]
    node _T_2777 = or(_T_2776, _T_2661) @[Decode.scala 14:30]
    node _T_2778 = or(_T_2777, _T_2665) @[Decode.scala 14:30]
    node _T_2779 = or(_T_2778, _T_2669) @[Decode.scala 14:30]
    node _T_2780 = or(_T_2779, _T_2673) @[Decode.scala 14:30]
    node _T_2781 = or(_T_2780, _T_2677) @[Decode.scala 14:30]
    node _T_2782 = or(_T_2781, _T_2681) @[Decode.scala 14:30]
    node _T_2783 = or(_T_2782, _T_2685) @[Decode.scala 14:30]
    node _T_2784 = or(_T_2783, _T_2689) @[Decode.scala 14:30]
    node _T_2785 = or(_T_2784, _T_2693) @[Decode.scala 14:30]
    node _T_2786 = or(_T_2785, _T_2697) @[Decode.scala 14:30]
    node _T_2787 = or(_T_2786, _T_2701) @[Decode.scala 14:30]
    node _T_2788 = or(_T_2787, _T_2703) @[Decode.scala 14:30]
    node _T_2789 = or(_T_2788, _T_2707) @[Decode.scala 14:30]
    node _T_2790 = or(_T_2789, _T_2711) @[Decode.scala 14:30]
    node _T_2791 = or(_T_2790, _T_2715) @[Decode.scala 14:30]
    node _T_2792 = or(_T_2791, _T_2719) @[Decode.scala 14:30]
    node _T_2793 = or(_T_2792, _T_2723) @[Decode.scala 14:30]
    node _T_2794 = or(_T_2793, _T_2727) @[Decode.scala 14:30]
    node _T_2795 = or(_T_2794, _T_2731) @[Decode.scala 14:30]
    node _T_2796 = or(_T_2795, _T_2733) @[Decode.scala 14:30]
    node _T_2797 = or(_T_2796, _T_2737) @[Decode.scala 14:30]
    node _T_2798 = or(_T_2797, _T_2741) @[Decode.scala 14:30]
    node _T_2799 = or(_T_2798, _T_2745) @[Decode.scala 14:30]
    node _T_2800 = or(_T_2799, _T_2749) @[Decode.scala 14:30]
    node _T_2801 = or(_T_2800, _T_2753) @[Decode.scala 14:30]
    node _T_2802 = or(_T_2801, _T_2757) @[Decode.scala 14:30]
    node id_ctrl_legal = or(_T_2802, _T_2761) @[Decode.scala 14:30]
    node _T_2817 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h74")) @[Decode.scala 13:65]
    node id_ctrl_branch = eq(_T_2817, UInt<32>("h60")) @[Decode.scala 13:121]
    skip
    node _T_2829 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h203c")) @[Decode.scala 13:65]
    node id_ctrl_jalr = eq(_T_2829, UInt<32>("h24")) @[Decode.scala 13:121]
    skip
    node _T_2882 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h58")) @[Decode.scala 13:65]
    node _T_2884 = eq(_T_2882, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2886 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h20")) @[Decode.scala 13:65]
    node _T_2888 = eq(_T_2886, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2890 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hc")) @[Decode.scala 13:65]
    node _T_2892 = eq(_T_2890, UInt<32>("h4")) @[Decode.scala 13:121]
    node _T_2894 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h48")) @[Decode.scala 13:65]
    node _T_2896 = eq(_T_2894, UInt<32>("h48")) @[Decode.scala 13:121]
    node _T_2898 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4050")) @[Decode.scala 13:65]
    node _T_2900 = eq(_T_2898, UInt<32>("h4050")) @[Decode.scala 13:121]
    skip
    node _T_2903 = or(_T_2884, _T_2888) @[Decode.scala 14:30]
    node _T_2904 = or(_T_2903, _T_2892) @[Decode.scala 14:30]
    node _T_2905 = or(_T_2904, _T_2896) @[Decode.scala 14:30]
    node _T_2906 = or(_T_2905, _T_2900) @[Decode.scala 14:30]
    skip
    node _T_2910 = eq(_T_2894, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2912 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18")) @[Decode.scala 13:65]
    node _T_2914 = eq(_T_2912, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2916 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4008")) @[Decode.scala 13:65]
    node _T_2918 = eq(_T_2916, UInt<32>("h4000")) @[Decode.scala 13:121]
    skip
    node _T_2921 = or(_T_2910, _T_2858) @[Decode.scala 14:30]
    node _T_2922 = or(_T_2921, _T_2914) @[Decode.scala 14:30]
    node _T_2923 = or(_T_2922, _T_2918) @[Decode.scala 14:30]
    node id_ctrl_sel_alu2 = cat(_T_2923, _T_2906) @[Cat.scala 30:58]
    node _T_2926 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4004")) @[Decode.scala 13:65]
    node _T_2928 = eq(_T_2926, UInt<32>("h0")) @[Decode.scala 13:121]
    skip
    node _T_2932 = eq(_T_3242, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2934 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h24")) @[Decode.scala 13:65]
    node _T_2936 = eq(_T_2934, UInt<32>("h0")) @[Decode.scala 13:121]
    skip
    node _T_2939 = or(_T_2928, _T_2932) @[Decode.scala 14:30]
    node _T_2940 = or(_T_2939, _T_2858) @[Decode.scala 14:30]
    node _T_2941 = or(_T_2940, _T_2936) @[Decode.scala 14:30]
    node _T_2942 = or(_T_2941, _T_2914) @[Decode.scala 14:30]
    skip
    node _T_2946 = eq(_T_2839, UInt<32>("h14")) @[Decode.scala 13:121]
    skip
    node _T_2949 = or(_T_2946, _T_2896) @[Decode.scala 14:30]
    node id_ctrl_sel_alu1 = cat(_T_2949, _T_2942) @[Cat.scala 30:58]
    skip
    node _T_2954 = eq(_T_2912, UInt<32>("h8")) @[Decode.scala 13:121]
    skip
    node _T_2958 = eq(_T_2856, UInt<32>("h40")) @[Decode.scala 13:121]
    skip
    node _T_2961 = or(_T_2954, _T_2958) @[Decode.scala 14:30]
    node _T_2963 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h14")) @[Decode.scala 13:65]
    node _T_2965 = eq(_T_2963, UInt<32>("h14")) @[Decode.scala 13:121]
    skip
    node _T_2968 = or(_T_2954, _T_2965) @[Decode.scala 14:30]
    node _T_2970 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h30")) @[Decode.scala 13:65]
    node _T_2972 = eq(_T_2970, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2974 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h201c")) @[Decode.scala 13:65]
    node _T_2976 = eq(_T_2974, UInt<32>("h4")) @[Decode.scala 13:121]
    skip
    node _T_2980 = eq(_T_2963, UInt<32>("h10")) @[Decode.scala 13:121]
    skip
    node _T_2983 = or(_T_2972, _T_2976) @[Decode.scala 14:30]
    node _T_2984 = or(_T_2983, _T_2980) @[Decode.scala 14:30]
    skip
    node id_ctrl_sel_imm = cat(cat(_T_2984, _T_2968), _T_2961) @[Cat.scala 30:58]
    node _T_2988 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10")) @[Decode.scala 13:65]
    node _T_2990 = eq(_T_2988, UInt<32>("h0")) @[Decode.scala 13:121]
    node _T_2992 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h8")) @[Decode.scala 13:65]
    node _T_2994 = eq(_T_2992, UInt<32>("h0")) @[Decode.scala 13:121]
    skip
    node id_ctrl_alu_dw = or(_T_2990, _T_2994) @[Decode.scala 14:30]
    node _T_2999 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3054")) @[Decode.scala 13:65]
    node _T_3001 = eq(_T_2999, UInt<32>("h1010")) @[Decode.scala 13:121]
    node _T_3003 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1058")) @[Decode.scala 13:65]
    node _T_3005 = eq(_T_3003, UInt<32>("h1040")) @[Decode.scala 13:121]
    node _T_3007 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7044")) @[Decode.scala 13:65]
    node _T_3009 = eq(_T_3007, UInt<32>("h7000")) @[Decode.scala 13:121]
    skip
    node _T_3012 = or(_T_3001, _T_3005) @[Decode.scala 14:30]
    node _T_3013 = or(_T_3012, _T_3009) @[Decode.scala 14:30]
    node _T_3015 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4054")) @[Decode.scala 13:65]
    node _T_3017 = eq(_T_3015, UInt<32>("h40")) @[Decode.scala 13:121]
    node _T_3019 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2058")) @[Decode.scala 13:65]
    node _T_3021 = eq(_T_3019, UInt<32>("h2040")) @[Decode.scala 13:121]
    skip
    node _T_3025 = eq(_T_2999, UInt<32>("h3010")) @[Decode.scala 13:121]
    node _T_3027 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h6054")) @[Decode.scala 13:65]
    node _T_3029 = eq(_T_3027, UInt<32>("h6010")) @[Decode.scala 13:121]
    node _T_3031 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40003034")) @[Decode.scala 13:65]
    node _T_3033 = eq(_T_3031, UInt<32>("h40000030")) @[Decode.scala 13:121]
    node _T_3035 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40001054")) @[Decode.scala 13:65]
    node _T_3037 = eq(_T_3035, UInt<32>("h40001010")) @[Decode.scala 13:121]
    skip
    node _T_3040 = or(_T_3017, _T_3021) @[Decode.scala 14:30]
    node _T_3041 = or(_T_3040, _T_3025) @[Decode.scala 14:30]
    node _T_3042 = or(_T_3041, _T_3029) @[Decode.scala 14:30]
    node _T_3043 = or(_T_3042, _T_3033) @[Decode.scala 14:30]
    node _T_3044 = or(_T_3043, _T_3037) @[Decode.scala 14:30]
    node _T_3046 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2054")) @[Decode.scala 13:65]
    node _T_3048 = eq(_T_3046, UInt<32>("h2010")) @[Decode.scala 13:121]
    node _T_3050 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40004054")) @[Decode.scala 13:65]
    node _T_3052 = eq(_T_3050, UInt<32>("h4010")) @[Decode.scala 13:121]
    node _T_3054 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h5054")) @[Decode.scala 13:65]
    node _T_3056 = eq(_T_3054, UInt<32>("h4010")) @[Decode.scala 13:121]
    node _T_3058 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4058")) @[Decode.scala 13:65]
    node _T_3060 = eq(_T_3058, UInt<32>("h4040")) @[Decode.scala 13:121]
    skip
    node _T_3063 = or(_T_3048, _T_3052) @[Decode.scala 14:30]
    node _T_3064 = or(_T_3063, _T_3056) @[Decode.scala 14:30]
    node _T_3065 = or(_T_3064, _T_3060) @[Decode.scala 14:30]
    skip
    node _T_3069 = eq(_T_3027, UInt<32>("h2010")) @[Decode.scala 13:121]
    node _T_3071 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40003054")) @[Decode.scala 13:65]
    node _T_3073 = eq(_T_3071, UInt<32>("h40001010")) @[Decode.scala 13:121]
    skip
    node _T_3076 = or(_T_3069, _T_3060) @[Decode.scala 14:30]
    node _T_3077 = or(_T_3076, _T_3033) @[Decode.scala 14:30]
    node _T_3078 = or(_T_3077, _T_3073) @[Decode.scala 14:30]
    skip
    skip
    node id_ctrl_alu_fn = cat(cat(_T_3078, _T_3065), cat(_T_3044, _T_3013)) @[Cat.scala 30:58]
    node _T_3104 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2008")) @[Decode.scala 13:65]
    node _T_3106 = eq(_T_3104, UInt<32>("h8")) @[Decode.scala 13:121]
    skip
    node _T_3110 = eq(_T_3250, UInt<32>("h20")) @[Decode.scala 13:121]
    node _T_3112 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18000020")) @[Decode.scala 13:65]
    node _T_3114 = eq(_T_3112, UInt<32>("h18000020")) @[Decode.scala 13:121]
    node _T_3116 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h20000020")) @[Decode.scala 13:65]
    node _T_3118 = eq(_T_3116, UInt<32>("h20000020")) @[Decode.scala 13:121]
    skip
    node _T_3121 = or(_T_3106, _T_3110) @[Decode.scala 14:30]
    node _T_3122 = or(_T_3121, _T_3114) @[Decode.scala 14:30]
    node _T_3123 = or(_T_3122, _T_3118) @[Decode.scala 14:30]
    node _T_3125 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10002008")) @[Decode.scala 13:65]
    node _T_3127 = eq(_T_3125, UInt<32>("h10002008")) @[Decode.scala 13:121]
    node _T_3129 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40002008")) @[Decode.scala 13:65]
    node _T_3131 = eq(_T_3129, UInt<32>("h40002008")) @[Decode.scala 13:121]
    skip
    node _T_3134 = or(_T_3127, _T_3131) @[Decode.scala 14:30]
    node _T_3136 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h8000008")) @[Decode.scala 13:65]
    node _T_3138 = eq(_T_3136, UInt<32>("h8000008")) @[Decode.scala 13:121]
    node _T_3140 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10000008")) @[Decode.scala 13:65]
    node _T_3142 = eq(_T_3140, UInt<32>("h10000008")) @[Decode.scala 13:121]
    node _T_3144 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h80000008")) @[Decode.scala 13:65]
    node _T_3146 = eq(_T_3144, UInt<32>("h80000008")) @[Decode.scala 13:121]
    skip
    node _T_3149 = or(_T_3106, _T_3138) @[Decode.scala 14:30]
    node _T_3150 = or(_T_3149, _T_3142) @[Decode.scala 14:30]
    node _T_3151 = or(_T_3150, _T_3146) @[Decode.scala 14:30]
    node _T_3153 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18002008")) @[Decode.scala 13:65]
    node _T_3155 = eq(_T_3153, UInt<32>("h2008")) @[Decode.scala 13:121]
    skip
    skip
    skip
    skip
    node id_ctrl_mem_cmd = cat(cat(cat(UInt<1>("h0"), _T_3155), _T_3151), cat(_T_3134, _T_3123)) @[Cat.scala 30:58]
    node _T_3164 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1000")) @[Decode.scala 13:65]
    node _T_3166 = eq(_T_3164, UInt<32>("h1000")) @[Decode.scala 13:121]
    skip
    node _T_3170 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2000")) @[Decode.scala 13:65]
    node _T_3172 = eq(_T_3170, UInt<32>("h2000")) @[Decode.scala 13:121]
    skip
    node _T_3176 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4000")) @[Decode.scala 13:65]
    node _T_3178 = eq(_T_3176, UInt<32>("h4000")) @[Decode.scala 13:121]
    skip
    skip
    node id_ctrl_mem_type = cat(cat(_T_3178, _T_3172), _T_3166) @[Cat.scala 30:58]
    node _T_3184 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h80000060")) @[Decode.scala 13:65]
    node _T_3186 = eq(_T_3184, UInt<32>("h40")) @[Decode.scala 13:121]
    node _T_3188 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10000060")) @[Decode.scala 13:65]
    skip
    node _T_3192 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h70")) @[Decode.scala 13:65]
    node id_ctrl_rfs3 = eq(_T_3192, UInt<32>("h40")) @[Decode.scala 13:121]
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
    node _T_3219 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3c")) @[Decode.scala 13:65]
    node _T_3221 = eq(_T_3219, UInt<32>("h4")) @[Decode.scala 13:121]
    skip
    node _T_3225 = eq(_T_3188, UInt<32>("h10000040")) @[Decode.scala 13:121]
    skip
    node _T_3228 = or(_T_3221, _T_3186) @[Decode.scala 14:30]
    node _T_3229 = or(_T_3228, id_ctrl_rfs3) @[Decode.scala 14:30]
    node id_ctrl_wfd = or(_T_3229, _T_3225) @[Decode.scala 14:30]
    node _T_3312 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h105c")) @[Decode.scala 13:65]
    node _T_3314 = eq(_T_3312, UInt<32>("h1004")) @[Decode.scala 13:121]
    node _T_3316 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2000060")) @[Decode.scala 13:65]
    node _T_3318 = eq(_T_3316, UInt<32>("h2000040")) @[Decode.scala 13:121]
    node _T_3320 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hd0000070")) @[Decode.scala 13:65]
    node _T_3322 = eq(_T_3320, UInt<32>("h40000050")) @[Decode.scala 13:121]
    skip
    node _T_3325 = or(_T_3314, _T_3318) @[Decode.scala 14:30]
    node id_ctrl_dp = or(_T_3325, _T_3322) @[Decode.scala 14:30]
    node _T_3335 = eq(ibuf.io_inst_0_bits_inst_rs1, UInt<5>("h0")) @[Rocket.scala 689:45]
    skip
    node _T_3338 = ibuf.io_inst_0_bits_inst_rs1 @[Rocket.scala 683:44]
    skip
    node _T_3341 = _T_3331._T_3340.data @[Rocket.scala 689:25]
    skip
    skip
    node _T_3348 = ibuf.io_inst_0_bits_inst_rs2 @[Rocket.scala 683:44]
    skip
    node _T_3351 = _T_3331._T_3350.data @[Rocket.scala 689:25]
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
    node id_system_insn = geq(id_ctrl_csr, UInt<3>("h4")) @[Rocket.scala 189:36]
    skip
    skip
    skip
    skip
    node id_csr_ren = and(_T_3453, _T_3335) @[Rocket.scala 190:54]
    node id_csr = mux(id_csr_ren, UInt<3>("h5"), id_ctrl_csr) @[Rocket.scala 191:19]
    node _T_3464 = eq(id_csr_ren, UInt<1>("h0")) @[Rocket.scala 192:54]
    node _T_3465 = and(id_csr_en, _T_3464) @[Rocket.scala 192:51]
    node _T_3466 = and(_T_3465, csr.io_decode_write_flush) @[Rocket.scala 192:66]
    node id_csr_flush = or(id_system_insn, _T_3466) @[Rocket.scala 192:37]
    skip
    node _T_3468 = eq(id_ctrl_legal, UInt<1>("h0")) @[Rocket.scala 194:25]
    node _T_3469 = bits(csr.io_status_isa, 12, 12) @[Rocket.scala 195:38]
    node _T_3471 = eq(_T_3469, UInt<1>("h0")) @[Rocket.scala 195:20]
    node _T_3472 = and(id_ctrl_div, _T_3471) @[Rocket.scala 195:17]
    node _T_3473 = or(_T_3468, _T_3472) @[Rocket.scala 194:40]
    node _T_3474 = bits(csr.io_status_isa, 0, 0) @[Rocket.scala 196:38]
    node _T_3476 = eq(_T_3474, UInt<1>("h0")) @[Rocket.scala 196:20]
    node _T_3477 = and(id_ctrl_amo, _T_3476) @[Rocket.scala 196:17]
    node _T_3478 = or(_T_3473, _T_3477) @[Rocket.scala 195:48]
    node _T_3479 = or(csr.io_decode_fp_illegal, io_fpu_illegal_rm) @[Rocket.scala 197:45]
    node _T_3480 = and(id_ctrl_fp, _T_3479) @[Rocket.scala 197:16]
    node _T_3481 = or(_T_3478, _T_3480) @[Rocket.scala 196:48]
    node _T_3482 = bits(csr.io_status_isa, 3, 3) @[Rocket.scala 198:37]
    node _T_3484 = eq(_T_3482, UInt<1>("h0")) @[Rocket.scala 198:19]
    skip
    node _T_3485 = and(id_ctrl_dp, _T_3484) @[Rocket.scala 198:16]
    node _T_3486 = or(_T_3481, _T_3485) @[Rocket.scala 197:67]
    node _T_3487 = bits(csr.io_status_isa, 2, 2) @[Rocket.scala 199:51]
    node _T_3489 = eq(_T_3487, UInt<1>("h0")) @[Rocket.scala 199:33]
    node _T_3490 = and(ibuf.io_inst_0_bits_rvc, _T_3489) @[Rocket.scala 199:30]
    node _T_3491 = or(_T_3486, _T_3490) @[Rocket.scala 198:47]
    skip
    skip
    skip
    node _T_3496 = and(_T_3464, csr.io_decode_write_illegal) @[Rocket.scala 201:61]
    node _T_3497 = or(csr.io_decode_read_illegal, _T_3496) @[Rocket.scala 201:46]
    node _T_3498 = and(id_csr_en, _T_3497) @[Rocket.scala 201:15]
    node _T_3499 = or(_T_3491, _T_3498) @[Rocket.scala 200:48]
    node _T_3500 = and(id_system_insn, csr.io_decode_system_illegal) @[Rocket.scala 202:20]
    node id_illegal_insn = or(_T_3499, _T_3500) @[Rocket.scala 201:93]
    node id_amo_rl = bits(ibuf.io_inst_0_bits_inst_bits, 25, 25) @[Rocket.scala 205:29]
    node _T_3501 = and(id_ctrl_amo, id_amo_rl) @[Rocket.scala 206:52]
    node id_fence_next = or(id_ctrl_fence, _T_3501) @[Rocket.scala 206:37]
    node _T_3511 = and(id_reg_fence, id_mem_busy) @[Rocket.scala 211:49]
    node _T_3512 = or(id_fence_next, _T_3511) @[Rocket.scala 211:33]
    node id_xcpt_if = or(ibuf.io_inst_0_bits_pf0, ibuf.io_inst_0_bits_pf1) @[Rocket.scala 221:45]
    node _T_3552 = or(csr.io_interrupt, bpu.io_debug_if) @[Rocket.scala 645:26]
    node _T_3553 = or(_T_3552, bpu.io_xcpt_if) @[Rocket.scala 645:26]
    node _T_3554 = or(_T_3553, id_xcpt_if) @[Rocket.scala 645:26]
    node id_xcpt = or(_T_3554, id_illegal_insn) @[Rocket.scala 645:26]
    node _T_3555 = mux(id_xcpt_if, UInt<2>("h1"), UInt<2>("h2")) @[Mux.scala 31:69]
    node _T_3556 = mux(bpu.io_xcpt_if, UInt<2>("h3"), _T_3555) @[Mux.scala 31:69]
    node _T_3557 = mux(bpu.io_debug_if, UInt<4>("hd"), pad(_T_3556, 4)) @[Mux.scala 31:69]
    node id_cause = mux(csr.io_interrupt, csr.io_interrupt_cause, pad(_T_3557, 64)) @[Mux.scala 31:69]
    node _T_3561 = and(ex_reg_valid, ex_ctrl_wxd) @[Rocket.scala 240:19]
    node _T_3562 = and(mem_reg_valid, mem_ctrl_wxd) @[Rocket.scala 241:20]
    node _T_3564 = eq(mem_ctrl_mem, UInt<1>("h0")) @[Rocket.scala 241:39]
    node _T_3565 = and(_T_3562, _T_3564) @[Rocket.scala 241:36]
    skip
    node id_bypass_src_0_0 = eq(UInt<5>("h0"), ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 243:82]
    skip
    node _T_3568 = eq(ex_waddr, ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 243:82]
    node id_bypass_src_0_1 = and(_T_3561, _T_3568) @[Rocket.scala 243:74]
    node _T_3569 = eq(mem_waddr, ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 243:82]
    node id_bypass_src_0_2 = and(_T_3565, _T_3569) @[Rocket.scala 243:74]
    skip
    node id_bypass_src_0_3 = and(_T_3562, _T_3569) @[Rocket.scala 243:74]
    node id_bypass_src_1_0 = eq(UInt<5>("h0"), ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 243:82]
    skip
    node _T_3572 = eq(ex_waddr, ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 243:82]
    node id_bypass_src_1_1 = and(_T_3561, _T_3572) @[Rocket.scala 243:74]
    node _T_3573 = eq(mem_waddr, ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 243:82]
    node id_bypass_src_1_2 = and(_T_3565, _T_3573) @[Rocket.scala 243:74]
    skip
    node id_bypass_src_1_3 = and(_T_3562, _T_3573) @[Rocket.scala 243:74]
    reg ex_reg_rs_bypass_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_bypass_0) @[Rocket.scala 247:29]
    reg ex_reg_rs_bypass_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_bypass_1) @[Rocket.scala 247:29]
    reg ex_reg_rs_lsb_0 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_lsb_0) @[Rocket.scala 248:26]
    reg ex_reg_rs_lsb_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_lsb_1) @[Rocket.scala 248:26]
    reg ex_reg_rs_msb_0 : UInt<62>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_msb_0) @[Rocket.scala 249:26]
    reg ex_reg_rs_msb_1 : UInt<62>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_msb_1) @[Rocket.scala 249:26]
    node _T_3605 = cat(ex_reg_rs_msb_0, ex_reg_rs_lsb_0) @[Cat.scala 30:58]
    skip
    skip
    skip
    node _GEN_1 = mux(eq(UInt<2>("h1"), ex_reg_rs_lsb_0), mem_reg_wdata, UInt<64>("h0")) @[Rocket.scala 251:14]
    skip
    node _GEN_2 = mux(eq(UInt<2>("h2"), ex_reg_rs_lsb_0), wb_reg_wdata, _GEN_1) @[Rocket.scala 251:14]
    skip
    node _GEN_3 = mux(eq(UInt<2>("h3"), ex_reg_rs_lsb_0), io_dmem_resp_bits_data_word_bypass, _GEN_2) @[Rocket.scala 251:14]
    skip
    node ex_rs_0 = mux(ex_reg_rs_bypass_0, _GEN_3, _T_3605) @[Rocket.scala 251:14]
    node _T_3607 = cat(ex_reg_rs_msb_1, ex_reg_rs_lsb_1) @[Cat.scala 30:58]
    skip
    node _GEN_5 = mux(eq(UInt<2>("h1"), ex_reg_rs_lsb_1), mem_reg_wdata, UInt<64>("h0")) @[Rocket.scala 251:14]
    node _GEN_6 = mux(eq(UInt<2>("h2"), ex_reg_rs_lsb_1), wb_reg_wdata, _GEN_5) @[Rocket.scala 251:14]
    node _GEN_7 = mux(eq(UInt<2>("h3"), ex_reg_rs_lsb_1), io_dmem_resp_bits_data_word_bypass, _GEN_6) @[Rocket.scala 251:14]
    skip
    node ex_rs_1 = mux(ex_reg_rs_bypass_1, _GEN_7, _T_3607) @[Rocket.scala 251:14]
    node _T_3609 = eq(ex_ctrl_sel_imm, UInt<3>("h5")) @[Rocket.scala 704:24]
    node _T_3611 = bits(ex_reg_inst, 31, 31) @[Rocket.scala 704:48]
    node _T_3612 = asSInt(_T_3611) @[Rocket.scala 704:53]
    node _T_3613 = mux(_T_3609, SInt<1>("h0"), _T_3612) @[Rocket.scala 704:19]
    node _T_3615 = eq(ex_ctrl_sel_imm, UInt<3>("h2")) @[Rocket.scala 705:26]
    node _T_3616 = bits(ex_reg_inst, 30, 20) @[Rocket.scala 705:41]
    node _T_3617 = asSInt(_T_3616) @[Rocket.scala 705:49]
    node _T_3618 = mux(_T_3615, _T_3617, pad(_T_3613, 11)) @[Rocket.scala 705:21]
    node _T_3620 = neq(ex_ctrl_sel_imm, UInt<3>("h2")) @[Rocket.scala 706:26]
    node _T_3622 = neq(ex_ctrl_sel_imm, UInt<3>("h3")) @[Rocket.scala 706:43]
    node _T_3623 = and(_T_3620, _T_3622) @[Rocket.scala 706:36]
    node _T_3624 = bits(ex_reg_inst, 19, 12) @[Rocket.scala 706:65]
    node _T_3625 = asSInt(_T_3624) @[Rocket.scala 706:73]
    node _T_3626 = mux(_T_3623, pad(_T_3613, 8), _T_3625) @[Rocket.scala 706:21]
    skip
    skip
    node _T_3631 = or(_T_3615, _T_3609) @[Rocket.scala 707:33]
    node _T_3634 = eq(ex_ctrl_sel_imm, UInt<3>("h3")) @[Rocket.scala 708:23]
    node _T_3635 = bits(ex_reg_inst, 20, 20) @[Rocket.scala 708:39]
    node _T_3636 = asSInt(_T_3635) @[Rocket.scala 708:44]
    node _T_3638 = eq(ex_ctrl_sel_imm, UInt<3>("h1")) @[Rocket.scala 709:23]
    node _T_3639 = bits(ex_reg_inst, 7, 7) @[Rocket.scala 709:39]
    node _T_3640 = asSInt(_T_3639) @[Rocket.scala 709:43]
    node _T_3641 = mux(_T_3638, _T_3640, _T_3613) @[Rocket.scala 709:18]
    node _T_3642 = mux(_T_3634, _T_3636, _T_3641) @[Rocket.scala 708:18]
    node _T_3643 = mux(_T_3631, SInt<1>("h0"), _T_3642) @[Rocket.scala 707:18]
    skip
    skip
    skip
    node _T_3650 = bits(ex_reg_inst, 30, 25) @[Rocket.scala 710:66]
    node _T_3651 = mux(_T_3631, UInt<6>("h0"), _T_3650) @[Rocket.scala 710:20]
    skip
    node _T_3656 = eq(ex_ctrl_sel_imm, UInt<3>("h0")) @[Rocket.scala 712:24]
    skip
    node _T_3659 = or(_T_3656, _T_3638) @[Rocket.scala 712:34]
    node _T_3660 = bits(ex_reg_inst, 11, 8) @[Rocket.scala 712:57]
    skip
    node _T_3663 = bits(ex_reg_inst, 19, 16) @[Rocket.scala 713:39]
    node _T_3664 = bits(ex_reg_inst, 24, 21) @[Rocket.scala 713:52]
    node _T_3665 = mux(_T_3609, _T_3663, _T_3664) @[Rocket.scala 713:19]
    node _T_3666 = mux(_T_3659, _T_3660, _T_3665) @[Rocket.scala 712:19]
    node _T_3667 = mux(_T_3615, UInt<4>("h0"), _T_3666) @[Rocket.scala 711:19]
    skip
    skip
    node _T_3672 = eq(ex_ctrl_sel_imm, UInt<3>("h4")) @[Rocket.scala 715:22]
    skip
    skip
    node _T_3676 = bits(ex_reg_inst, 15, 15) @[Rocket.scala 716:37]
    node _T_3678 = and(_T_3609, _T_3676) @[Rocket.scala 716:17]
    node _T_3679 = mux(_T_3672, _T_3635, _T_3678) @[Rocket.scala 715:17]
    node _T_3680 = mux(_T_3656, _T_3639, _T_3679) @[Rocket.scala 714:17]
    skip
    skip
    node _T_3683 = asUInt(_T_3643) @[Cat.scala 30:58]
    node _T_3684 = asUInt(_T_3626) @[Cat.scala 30:58]
    skip
    node _T_3686 = asUInt(_T_3618) @[Cat.scala 30:58]
    node _T_3687 = asUInt(_T_3613) @[Cat.scala 30:58]
    skip
    skip
    node _T_3690 = cat(cat(cat(_T_3687, _T_3686), cat(_T_3684, _T_3683)), cat(cat(_T_3651, _T_3667), _T_3680)) @[Cat.scala 30:58]
    node ex_imm = asSInt(_T_3690) @[Rocket.scala 718:53]
    node _T_3693 = asSInt(ex_rs_0) @[Rocket.scala 254:24]
    node _T_3695 = asSInt(ex_reg_pc) @[Rocket.scala 255:24]
    node _T_3696 = eq(UInt<2>("h2"), ex_ctrl_sel_alu1) @[Mux.scala 46:19]
    node _T_3697 = mux(_T_3696, _T_3695, SInt<40>("h0")) @[Mux.scala 46:16]
    node _T_3698 = eq(UInt<2>("h1"), ex_ctrl_sel_alu1) @[Mux.scala 46:19]
    node ex_op1 = mux(_T_3698, _T_3693, pad(_T_3697, 64)) @[Mux.scala 46:16]
    node _T_3701 = asSInt(ex_rs_1) @[Rocket.scala 257:24]
    node _T_3706 = mux(ex_reg_rvc, SInt<4>("h2"), SInt<4>("h4")) @[Rocket.scala 259:19]
    node _T_3707 = eq(UInt<2>("h1"), ex_ctrl_sel_alu2) @[Mux.scala 46:19]
    node _T_3708 = mux(_T_3707, _T_3706, SInt<4>("h0")) @[Mux.scala 46:16]
    node _T_3709 = eq(UInt<2>("h3"), ex_ctrl_sel_alu2) @[Mux.scala 46:19]
    node _T_3710 = mux(_T_3709, ex_imm, pad(_T_3708, 32)) @[Mux.scala 46:16]
    node _T_3711 = eq(UInt<2>("h2"), ex_ctrl_sel_alu2) @[Mux.scala 46:19]
    node ex_op2 = mux(_T_3711, _T_3701, pad(_T_3710, 64)) @[Mux.scala 46:16]
    skip
    skip
    skip
    skip
    node _T_3718 = eq(take_pc_mem_wb, UInt<1>("h0")) @[Rocket.scala 277:20]
    node _T_3719 = and(_T_3718, ibuf.io_inst_0_valid) @[Rocket.scala 277:29]
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
    node _T_3735 = eq(bpu.io_xcpt_if, UInt<1>("h0")) @[Rocket.scala 293:13]
    node _T_3737 = eq(ibuf.io_inst_0_bits_pf0, UInt<1>("h0")) @[Rocket.scala 293:32]
    node _T_3738 = and(_T_3735, _T_3737) @[Rocket.scala 293:29]
    node _T_3739 = and(_T_3738, ibuf.io_inst_0_bits_pf1) @[Rocket.scala 293:58]
    node _GEN_16 = mux(_T_3739, UInt<2>("h1"), UInt<2>("h0")) @[Rocket.scala 293:87]
    node _GEN_17 = or(_T_3739, ibuf.io_inst_0_bits_rvc) @[Rocket.scala 293:87]
    skip
    node _GEN_18 = mux(id_xcpt, UInt<4>("h0"), id_ctrl_alu_fn) @[Rocket.scala 288:20]
    skip
    node _GEN_19 = or(id_xcpt, id_ctrl_alu_dw) @[Rocket.scala 288:20]
    skip
    node _GEN_20 = mux(id_xcpt, UInt<2>("h2"), id_ctrl_sel_alu1) @[Rocket.scala 288:20]
    skip
    node _GEN_21 = mux(id_xcpt, _GEN_16, id_ctrl_sel_alu2) @[Rocket.scala 288:20]
    node _GEN_22 = mux(id_xcpt, _GEN_17, ibuf.io_inst_0_bits_rvc) @[Rocket.scala 288:20]
    node _T_3742 = or(id_ctrl_fence_i, id_csr_flush) @[Rocket.scala 298:42]
    node _T_3743 = or(_T_3742, csr.io_singleStep) @[Rocket.scala 298:58]
    skip
    node _T_3744 = and(id_ctrl_jalr, csr.io_status_debug) @[Rocket.scala 301:24]
    node _GEN_23 = or(_T_3744, _T_3743) @[Rocket.scala 301:48]
    node _GEN_24 = or(_T_3744, id_ctrl_fence_i) @[Rocket.scala 301:48]
    node _T_3747 = or(id_bypass_src_0_0, id_bypass_src_0_1) @[Rocket.scala 307:48]
    node _T_3748 = or(_T_3747, id_bypass_src_0_2) @[Rocket.scala 307:48]
    node _T_3749 = or(_T_3748, id_bypass_src_0_3) @[Rocket.scala 307:48]
    node _T_3754 = mux(id_bypass_src_0_2, UInt<2>("h2"), UInt<2>("h3")) @[Mux.scala 31:69]
    node _T_3755 = mux(id_bypass_src_0_1, UInt<2>("h1"), _T_3754) @[Mux.scala 31:69]
    node _T_3756 = mux(id_bypass_src_0_0, UInt<2>("h0"), _T_3755) @[Mux.scala 31:69]
    node _T_3758 = eq(_T_3749, UInt<1>("h0")) @[Rocket.scala 311:26]
    node _T_3759 = and(id_ctrl_rxs1, _T_3758) @[Rocket.scala 311:23]
    node _T_4187 = eq(replay_wb_common, UInt<1>("h0")) @[Rocket.scala 450:34]
    node _T_4188 = and(wb_reg_valid, _T_4187) @[Rocket.scala 450:31]
    node _T_4190 = eq(wb_reg_xcpt, UInt<1>("h0")) @[Rocket.scala 450:48]
    node wb_valid = and(_T_4188, _T_4190) @[Rocket.scala 450:45]
    node wb_wen = and(wb_valid, wb_ctrl_wxd) @[Rocket.scala 451:25]
    node dmem_resp_valid = and(io_dmem_resp_valid, io_dmem_resp_bits_has_data) @[Rocket.scala 426:44]
    node dmem_resp_replay = and(dmem_resp_valid, io_dmem_resp_bits_replay) @[Rocket.scala 427:42]
    node _T_4176 = bits(io_dmem_resp_bits_tag, 0, 0) @[Rocket.scala 423:45]
    skip
    node dmem_resp_xpu = eq(_T_4176, UInt<1>("h0")) @[Rocket.scala 423:23]
    node _T_4183 = and(dmem_resp_replay, dmem_resp_xpu) @[Rocket.scala 442:26]
    node _T_4182 = and(div.io_resp_ready, div.io_resp_valid) @[Decoupled.scala 30:37]
    node ll_wen = or(_T_4183, _T_4182) @[Rocket.scala 442:44]
    skip
    node rf_wen = or(wb_wen, ll_wen) @[Rocket.scala 452:23]
    node dmem_resp_waddr = bits(io_dmem_resp_bits_tag, 5, 1) @[Rocket.scala 425:46]
    node ll_waddr = mux(_T_4183, dmem_resp_waddr, div.io_resp_bits_tag) @[Rocket.scala 442:44]
    skip
    node rf_waddr = mux(ll_wen, ll_waddr, wb_waddr) @[Rocket.scala 453:21]
    node _T_4197 = neq(rf_waddr, UInt<5>("h0")) @[Rocket.scala 694:16]
    node _T_4201 = eq(rf_waddr, ibuf.io_inst_0_bits_inst_rs1) @[Rocket.scala 697:20]
    node _T_4191 = and(dmem_resp_valid, dmem_resp_xpu) @[Rocket.scala 454:38]
    node ll_wdata = div.io_resp_bits_data
    node _T_4193 = neq(wb_ctrl_csr, UInt<3>("h0")) @[Rocket.scala 456:34]
    node _T_4194 = mux(_T_4193, csr.io_rw_rdata, wb_reg_wdata) @[Rocket.scala 456:21]
    node _T_4195 = mux(ll_wen, ll_wdata, _T_4194) @[Rocket.scala 455:21]
    node rf_wdata = mux(_T_4191, io_dmem_resp_bits_data, _T_4195) @[Rocket.scala 454:21]
    node _GEN_156 = mux(_T_4201, rf_wdata, _T_3341) @[Rocket.scala 697:31]
    node _GEN_163 = mux(_T_4197, _GEN_156, _T_3341) @[Rocket.scala 694:29]
    node id_rs_0 = mux(rf_wen, _GEN_163, _T_3341) @[Rocket.scala 458:17]
    skip
    node _T_3760 = bits(id_rs_0, 1, 0) @[Rocket.scala 312:37]
    node _T_3761 = shr(id_rs_0, 2) @[Rocket.scala 313:38]
    node _GEN_25 = mux(_T_3759, _T_3760, _T_3756) @[Rocket.scala 311:38]
    node _GEN_26 = mux(_T_3759, _T_3761, ex_reg_rs_msb_0) @[Rocket.scala 311:38]
    node _T_3762 = or(id_bypass_src_1_0, id_bypass_src_1_1) @[Rocket.scala 307:48]
    node _T_3763 = or(_T_3762, id_bypass_src_1_2) @[Rocket.scala 307:48]
    node _T_3764 = or(_T_3763, id_bypass_src_1_3) @[Rocket.scala 307:48]
    node _T_3769 = mux(id_bypass_src_1_2, UInt<2>("h2"), UInt<2>("h3")) @[Mux.scala 31:69]
    node _T_3770 = mux(id_bypass_src_1_1, UInt<2>("h1"), _T_3769) @[Mux.scala 31:69]
    node _T_3771 = mux(id_bypass_src_1_0, UInt<2>("h0"), _T_3770) @[Mux.scala 31:69]
    node _T_3773 = eq(_T_3764, UInt<1>("h0")) @[Rocket.scala 311:26]
    node _T_3774 = and(id_ctrl_rxs2, _T_3773) @[Rocket.scala 311:23]
    node _T_4202 = eq(rf_waddr, ibuf.io_inst_0_bits_inst_rs2) @[Rocket.scala 697:20]
    node _GEN_157 = mux(_T_4202, rf_wdata, _T_3351) @[Rocket.scala 697:31]
    node _GEN_164 = mux(_T_4197, _GEN_157, _T_3351) @[Rocket.scala 694:29]
    node id_rs_1 = mux(rf_wen, _GEN_164, _T_3351) @[Rocket.scala 458:17]
    skip
    node _T_3775 = bits(id_rs_1, 1, 0) @[Rocket.scala 312:37]
    node _T_3776 = shr(id_rs_1, 2) @[Rocket.scala 313:38]
    node _GEN_27 = mux(_T_3774, _T_3775, _T_3771) @[Rocket.scala 311:38]
    node _GEN_28 = mux(_T_3774, _T_3776, ex_reg_rs_msb_1) @[Rocket.scala 311:38]
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
    node _T_4324 = and(mem_reg_valid, data_hazard_mem) @[Rocket.scala 506:32]
    node id_load_use = and(_T_4324, mem_ctrl_mem) @[Rocket.scala 506:51]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_3779 = or(_T_3444, csr.io_interrupt) @[Rocket.scala 317:21]
    node _T_3780 = or(_T_3779, ibuf.io_inst_0_bits_replay) @[Rocket.scala 317:41]
    skip
    skip
    node _T_3785 = eq(io_dmem_req_ready, UInt<1>("h0")) @[Rocket.scala 325:45]
    node _T_3786 = and(ex_ctrl_mem, _T_3785) @[Rocket.scala 325:42]
    node _T_3788 = eq(div.io_req_ready, UInt<1>("h0")) @[Rocket.scala 326:45]
    node _T_3789 = and(ex_ctrl_div, _T_3788) @[Rocket.scala 326:42]
    node replay_ex_structural = or(_T_3786, _T_3789) @[Rocket.scala 325:64]
    node replay_ex_load_use = and(wb_dcache_miss, ex_reg_load_use) @[Rocket.scala 327:43]
    node _T_3790 = or(replay_ex_structural, replay_ex_load_use) @[Rocket.scala 328:75]
    node _T_3791 = and(ex_reg_valid, _T_3790) @[Rocket.scala 328:50]
    node replay_ex = or(ex_reg_replay, _T_3791) @[Rocket.scala 328:33]
    node _T_3792 = or(take_pc_mem_wb, replay_ex) @[Rocket.scala 329:35]
    node _T_3794 = eq(ex_reg_valid, UInt<1>("h0")) @[Rocket.scala 329:51]
    node ctrl_killx = or(_T_3792, _T_3794) @[Rocket.scala 329:48]
    node _T_3796 = eq(ex_ctrl_mem_cmd, UInt<5>("h7")) @[Rocket.scala 331:40]
    skip
    node _T_3810 = eq(UInt<3>("h0"), ex_ctrl_mem_type) @[Rocket.scala 331:91]
    skip
    node _T_3811 = eq(UInt<3>("h4"), ex_ctrl_mem_type) @[Rocket.scala 331:91]
    skip
    node _T_3812 = eq(UInt<3>("h1"), ex_ctrl_mem_type) @[Rocket.scala 331:91]
    skip
    node _T_3813 = eq(UInt<3>("h5"), ex_ctrl_mem_type) @[Rocket.scala 331:91]
    skip
    node _T_3816 = or(_T_3810, _T_3811) @[Rocket.scala 331:91]
    node _T_3817 = or(_T_3816, _T_3812) @[Rocket.scala 331:91]
    node _T_3818 = or(_T_3817, _T_3813) @[Rocket.scala 331:91]
    node ex_slow_bypass = or(_T_3796, _T_3818) @[Rocket.scala 331:50]
    node ex_xcpt = or(ex_reg_xcpt_interrupt, ex_reg_xcpt) @[Rocket.scala 334:28]
    node _T_3819 = or(mem_reg_valid, mem_reg_replay) @[Rocket.scala 337:36]
    node mem_pc_valid = or(_T_3819, mem_reg_xcpt_interrupt) @[Rocket.scala 337:54]
    skip
    skip
    node _T_4037 = bits(mem_npc, 1, 1) @[Rocket.scala 345:66]
    node mem_npc_misaligned = and(_T_3489, _T_4037) @[Rocket.scala 345:56]
    node _T_4039 = eq(mem_reg_xcpt, UInt<1>("h0")) @[Rocket.scala 346:27]
    node _T_4040 = xor(mem_ctrl_jalr, mem_npc_misaligned) @[Rocket.scala 346:59]
    node _T_4041 = and(_T_4039, _T_4040) @[Rocket.scala 346:41]
    node _T_4042 = asSInt(mem_reg_wdata) @[Rocket.scala 346:111]
    node _T_4043 = mux(_T_4041, pad(mem_br_target, 64), _T_4042) @[Rocket.scala 346:26]
    node mem_int_wdata = asUInt(_T_4043) @[Rocket.scala 346:119]
    node _T_4044 = or(mem_ctrl_branch, mem_ctrl_jalr) @[Rocket.scala 347:33]
    node mem_cfi = or(_T_4044, mem_ctrl_jal) @[Rocket.scala 347:50]
    skip
    node _T_4046 = or(_T_3821, mem_ctrl_jalr) @[Rocket.scala 348:57]
    skip
    node mem_cfi_taken = or(_T_4046, mem_ctrl_jal) @[Rocket.scala 348:74]
    skip
    skip
    skip
    node _T_4054 = eq(ctrl_killx, UInt<1>("h0")) @[Rocket.scala 353:20]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_4065 = eq(ex_ctrl_mem_cmd, UInt<5>("h0")) @[Consts.scala 35:31]
    node _T_4067 = eq(ex_ctrl_mem_cmd, UInt<5>("h6")) @[Consts.scala 35:48]
    node _T_4068 = or(_T_4065, _T_4067) @[Consts.scala 35:41]
    skip
    node _T_4071 = or(_T_4068, _T_3796) @[Consts.scala 35:58]
    node _T_4072 = bits(ex_ctrl_mem_cmd, 3, 3) @[Consts.scala 33:29]
    node _T_4074 = eq(ex_ctrl_mem_cmd, UInt<5>("h4")) @[Consts.scala 33:40]
    node _T_4075 = or(_T_4072, _T_4074) @[Consts.scala 33:33]
    node _T_4076 = or(_T_4071, _T_4075) @[Consts.scala 35:75]
    node _T_4077 = and(ex_ctrl_mem, _T_4076) @[Rocket.scala 362:33]
    node _T_4079 = eq(ex_ctrl_mem_cmd, UInt<5>("h1")) @[Consts.scala 36:32]
    skip
    node _T_4082 = or(_T_4079, _T_3796) @[Consts.scala 36:42]
    skip
    skip
    skip
    node _T_4087 = or(_T_4082, _T_4075) @[Consts.scala 36:59]
    node _T_4088 = and(ex_ctrl_mem, _T_4087) @[Rocket.scala 363:34]
    node _GEN_68 = mux(ex_reg_btb_hit, ex_reg_btb_resp_taken, mem_reg_btb_resp_taken) @[Rocket.scala 365:27]
    node _GEN_69 = mux(ex_reg_btb_hit, ex_reg_btb_resp_mask, mem_reg_btb_resp_mask) @[Rocket.scala 365:27]
    node _GEN_70 = mux(ex_reg_btb_hit, ex_reg_btb_resp_bridx, mem_reg_btb_resp_bridx) @[Rocket.scala 365:27]
    node _GEN_71 = mux(ex_reg_btb_hit, ex_reg_btb_resp_target, mem_reg_btb_resp_target) @[Rocket.scala 365:27]
    node _GEN_72 = mux(ex_reg_btb_hit, ex_reg_btb_resp_entry, mem_reg_btb_resp_entry) @[Rocket.scala 365:27]
    node _GEN_73 = mux(ex_reg_btb_hit, ex_reg_btb_resp_bht_history, mem_reg_btb_resp_bht_history) @[Rocket.scala 365:27]
    node _GEN_74 = mux(ex_reg_btb_hit, ex_reg_btb_resp_bht_value, mem_reg_btb_resp_bht_value) @[Rocket.scala 365:27]
    skip
    node _T_4090 = and(ex_ctrl_rxs2, ex_ctrl_mem) @[Rocket.scala 372:24]
    node _GEN_75 = mux(_T_4090, ex_rs_1, mem_reg_rs2) @[Rocket.scala 372:58]
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
    node _T_4091 = and(mem_reg_load, bpu.io_xcpt_ld) @[Rocket.scala 377:38]
    node _T_4092 = and(mem_reg_store, bpu.io_xcpt_st) @[Rocket.scala 377:75]
    node mem_breakpoint = or(_T_4091, _T_4092) @[Rocket.scala 377:57]
    node _T_4093 = and(mem_reg_load, bpu.io_debug_ld) @[Rocket.scala 378:44]
    node _T_4094 = and(mem_reg_store, bpu.io_debug_st) @[Rocket.scala 378:82]
    node mem_debug_breakpoint = or(_T_4093, _T_4094) @[Rocket.scala 378:64]
    node _T_4126 = and(mem_ctrl_mem, io_dmem_xcpt_ma_st) @[Rocket.scala 383:19]
    node _T_4128 = and(mem_ctrl_mem, io_dmem_xcpt_ma_ld) @[Rocket.scala 384:19]
    node _T_4130 = and(mem_ctrl_mem, io_dmem_xcpt_pf_st) @[Rocket.scala 385:19]
    node _T_4132 = and(mem_ctrl_mem, io_dmem_xcpt_pf_ld) @[Rocket.scala 386:19]
    node _T_4134 = or(mem_debug_breakpoint, mem_breakpoint) @[Rocket.scala 645:26]
    node _T_4135 = or(_T_4134, mem_npc_misaligned) @[Rocket.scala 645:26]
    node _T_4136 = or(_T_4135, _T_4126) @[Rocket.scala 645:26]
    node _T_4137 = or(_T_4136, _T_4128) @[Rocket.scala 645:26]
    node _T_4138 = or(_T_4137, _T_4130) @[Rocket.scala 645:26]
    node mem_new_xcpt = or(_T_4138, _T_4132) @[Rocket.scala 645:26]
    node _T_4139 = mux(_T_4130, UInt<3>("h7"), UInt<3>("h5")) @[Mux.scala 31:69]
    node _T_4140 = mux(_T_4128, UInt<3>("h4"), _T_4139) @[Mux.scala 31:69]
    node _T_4141 = mux(_T_4126, UInt<3>("h6"), _T_4140) @[Mux.scala 31:69]
    node _T_4142 = mux(mem_npc_misaligned, UInt<3>("h0"), _T_4141) @[Mux.scala 31:69]
    node _T_4143 = mux(mem_breakpoint, UInt<3>("h3"), _T_4142) @[Mux.scala 31:69]
    node mem_new_cause = mux(mem_debug_breakpoint, UInt<4>("hd"), pad(_T_4143, 4)) @[Mux.scala 31:69]
    node _T_4144 = or(mem_reg_xcpt_interrupt, mem_reg_xcpt) @[Rocket.scala 389:29]
    node _T_4145 = and(mem_reg_valid, mem_new_xcpt) @[Rocket.scala 390:20]
    node mem_xcpt = or(_T_4144, _T_4145) @[Rocket.scala 645:26]
    node mem_cause = mux(_T_4144, mem_reg_cause, pad(mem_new_cause, 64)) @[Mux.scala 31:69]
    skip
    node dcache_kill_mem = and(_T_3562, io_dmem_replay_next) @[Rocket.scala 392:55]
    node _T_4147 = and(mem_reg_valid, mem_ctrl_fp) @[Rocket.scala 393:36]
    node fpu_kill_mem = and(_T_4147, io_fpu_nack_mem) @[Rocket.scala 393:51]
    node _T_4148 = or(dcache_kill_mem, mem_reg_replay) @[Rocket.scala 394:37]
    node replay_mem = or(_T_4148, fpu_kill_mem) @[Rocket.scala 394:55]
    node _T_4149 = or(dcache_kill_mem, take_pc_wb) @[Rocket.scala 395:38]
    node _T_4150 = or(_T_4149, mem_reg_xcpt) @[Rocket.scala 395:52]
    node _T_4152 = eq(mem_reg_valid, UInt<1>("h0")) @[Rocket.scala 395:71]
    node killm_common = or(_T_4150, _T_4152) @[Rocket.scala 395:68]
    skip
    reg _T_4154 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_4154) @[Rocket.scala 396:37]
    skip
    node _T_4156 = or(killm_common, mem_xcpt) @[Rocket.scala 397:33]
    node ctrl_killm = or(_T_4156, fpu_kill_mem) @[Rocket.scala 397:45]
    skip
    node _T_4160 = eq(take_pc_wb, UInt<1>("h0")) @[Rocket.scala 401:34]
    skip
    skip
    skip
    skip
    skip
    node _T_4167 = and(_T_4039, mem_ctrl_fp) @[Rocket.scala 406:39]
    node _T_4168 = and(_T_4167, mem_ctrl_wxd) @[Rocket.scala 406:54]
    node _T_4169 = mux(_T_4168, io_fpu_toint_data, mem_int_wdata) @[Rocket.scala 406:24]
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
    node _T_4204 = shr(wb_reg_wdata, 38) @[Rocket.scala 653:16]
    node _T_4205 = bits(wb_reg_wdata, 39, 38) @[Rocket.scala 654:15]
    node _T_4206 = asSInt(_T_4205) @[Rocket.scala 654:39]
    node _T_4208 = eq(_T_4204, UInt<26>("h0")) @[Rocket.scala 656:13]
    node _T_4210 = eq(_T_4204, UInt<26>("h1")) @[Rocket.scala 656:30]
    node _T_4211 = or(_T_4208, _T_4210) @[Rocket.scala 656:25]
    node _T_4213 = neq(_T_4206, SInt<2>("h0")) @[Rocket.scala 656:45]
    node _T_4214 = asSInt(_T_4204) @[Rocket.scala 657:13]
    node _T_4216 = eq(_T_4214, SInt<26>("h-1")) @[Rocket.scala 657:20]
    skip
    node _T_4219 = eq(_T_4214, SInt<26>("h-2")) @[Rocket.scala 657:45]
    node _T_4220 = or(_T_4216, _T_4219) @[Rocket.scala 657:33]
    node _T_4222 = eq(_T_4206, SInt<2>("h-1")) @[Rocket.scala 657:61]
    node _T_4223 = bits(_T_4206, 0, 0) @[Rocket.scala 657:76]
    node _T_4224 = mux(_T_4220, _T_4222, _T_4223) @[Rocket.scala 657:10]
    node _T_4225 = mux(_T_4211, _T_4213, _T_4224) @[Rocket.scala 656:10]
    node _T_4226 = bits(wb_reg_wdata, 38, 0) @[Rocket.scala 658:16]
    skip
    skip
    skip
    node _T_4246 = dshlw(UInt<32>("h1"), ll_waddr) @[Rocket.scala 672:62]
    node _T_4248 = mux(ll_wen, _T_4246, UInt<32>("h0")) @[Rocket.scala 672:49]
    node _T_4249 = not(_T_4248) @[Rocket.scala 664:64]
    node _T_4250 = and(_T_4243, _T_4249) @[Rocket.scala 664:62]
    skip
    node _GEN_172 = mux(ll_wen, _T_4250, _T_4241) @[Rocket.scala 676:18]
    node _T_4262 = and(wb_set_sboard, wb_wen) @[Rocket.scala 490:28]
    node _T_4264 = dshlw(UInt<32>("h1"), wb_waddr) @[Rocket.scala 672:62]
    node _T_4266 = mux(_T_4262, _T_4264, UInt<32>("h0")) @[Rocket.scala 672:49]
    node _T_4267 = or(_T_4250, _T_4266) @[Rocket.scala 663:60]
    node _T_4268 = or(ll_wen, _T_4262) @[Rocket.scala 675:17]
    node _GEN_173 = mux(_T_4268, _T_4267, _GEN_172) @[Rocket.scala 676:18]
    node _T_4350 = and(wb_dcache_miss, wb_ctrl_wfd) @[Rocket.scala 515:35]
    node _T_4351 = or(_T_4350, io_fpu_sboard_set) @[Rocket.scala 515:50]
    node _T_4352 = and(_T_4351, wb_valid) @[Rocket.scala 515:72]
    skip
    node _T_4356 = mux(_T_4352, _T_4264, UInt<32>("h0")) @[Rocket.scala 672:49]
    node _T_4357 = or(_T_4348, _T_4356) @[Rocket.scala 663:60]
    skip
    node _GEN_174 = mux(_T_4352, _T_4357, _T_4348) @[Rocket.scala 676:18]
    node _T_4359 = and(dmem_resp_replay, _T_4176) @[Rocket.scala 516:38]
    node _T_4361 = dshlw(UInt<32>("h1"), dmem_resp_waddr) @[Rocket.scala 672:62]
    node _T_4363 = mux(_T_4359, _T_4361, UInt<32>("h0")) @[Rocket.scala 672:49]
    node _T_4364 = not(_T_4363) @[Rocket.scala 664:64]
    node _T_4365 = and(_T_4357, _T_4364) @[Rocket.scala 664:62]
    node _T_4366 = or(_T_4352, _T_4359) @[Rocket.scala 675:17]
    node _GEN_175 = mux(_T_4366, _T_4365, _GEN_174) @[Rocket.scala 676:18]
    node _T_4368 = dshlw(UInt<32>("h1"), io_fpu_sboard_clra) @[Rocket.scala 672:62]
    node _T_4370 = mux(io_fpu_sboard_clr, _T_4368, UInt<32>("h0")) @[Rocket.scala 672:49]
    node _T_4371 = not(_T_4370) @[Rocket.scala 664:64]
    node _T_4372 = and(_T_4365, _T_4371) @[Rocket.scala 664:62]
    node _T_4373 = or(_T_4366, io_fpu_sboard_clr) @[Rocket.scala 675:17]
    node _GEN_176 = mux(_T_4373, _T_4372, _GEN_175) @[Rocket.scala 676:18]
    skip
    node _T_4395 = or(io_dmem_req_valid, dcache_blocked) @[Rocket.scala 523:62]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_4432 = or(wb_reg_xcpt, csr.io_eret) @[Rocket.scala 540:17]
    skip
    skip
    node _T_4436 = mux(replay_wb_common, wb_reg_pc, mem_npc) @[Rocket.scala 541:8]
    skip
    node _T_4438 = and(wb_reg_valid, wb_ctrl_fence_i) @[Rocket.scala 544:40]
    node _T_4440 = eq(io_dmem_s2_nack, UInt<1>("h0")) @[Rocket.scala 544:62]
    skip
    node _T_4443 = eq(ctrl_stalld, UInt<1>("h0")) @[Rocket.scala 547:28]
    skip
    node _T_4445 = and(mem_reg_replay, mem_reg_btb_hit) @[Rocket.scala 549:47]
    skip
    node _T_4448 = and(mem_reg_valid, _T_4160) @[Rocket.scala 549:85]
    node _T_4450 = eq(mem_cfi, UInt<1>("h0")) @[Rocket.scala 549:123]
    node _T_4451 = or(mem_cfi_taken, _T_4450) @[Rocket.scala 549:120]
    node _T_4452 = and(_T_4451, mem_misprediction) @[Rocket.scala 549:133]
    skip
    skip
    skip
    skip
    node _T_4459 = and(_T_4448, _T_4452) @[Rocket.scala 549:100]
    skip
    node _T_4462 = eq(mem_reg_replay, UInt<1>("h0")) @[Rocket.scala 550:38]
    skip
    skip
    node _T_4465 = bits(mem_reg_inst, 19, 15) @[Rocket.scala 552:68]
    node _T_4468 = and(_T_4465, UInt<5>("h1b")) @[Rocket.scala 552:76]
    node _T_4469 = eq(UInt<5>("h1"), _T_4468) @[Rocket.scala 552:76]
    skip
    node _T_4473 = mux(mem_reg_rvc, UInt<2>("h0"), UInt<2>("h2")) @[Rocket.scala 554:74]
    node _GEN_187 = pad(_T_4473, 40) @[Rocket.scala 554:69]
    node _T_4474 = add(mem_reg_pc, _GEN_187) @[Rocket.scala 554:69]
    node _T_4475 = tail(_T_4474, 1) @[Rocket.scala 554:69]
    node _T_4476 = not(io_imem_btb_update_bits_br_pc) @[Rocket.scala 555:35]
    node _T_4478 = or(_T_4476, UInt<39>("h3")) @[Rocket.scala 555:66]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_4487 = bits(mem_waddr, 0, 0) @[Rocket.scala 567:80]
    skip
    skip
    skip
    skip
    skip
    node ex_dcache_tag = cat(ex_waddr, ex_ctrl_fp) @[Cat.scala 30:58]
    node _T_4495 = shr(ex_rs_0, 38) @[Rocket.scala 653:16]
    node _T_4496 = bits(alu.io_adder_out, 39, 38) @[Rocket.scala 654:15]
    node _T_4497 = asSInt(_T_4496) @[Rocket.scala 654:39]
    node _T_4499 = eq(_T_4495, UInt<26>("h0")) @[Rocket.scala 656:13]
    node _T_4501 = eq(_T_4495, UInt<26>("h1")) @[Rocket.scala 656:30]
    node _T_4502 = or(_T_4499, _T_4501) @[Rocket.scala 656:25]
    node _T_4504 = neq(_T_4497, SInt<2>("h0")) @[Rocket.scala 656:45]
    node _T_4505 = asSInt(_T_4495) @[Rocket.scala 657:13]
    node _T_4507 = eq(_T_4505, SInt<26>("h-1")) @[Rocket.scala 657:20]
    skip
    node _T_4510 = eq(_T_4505, SInt<26>("h-2")) @[Rocket.scala 657:45]
    node _T_4511 = or(_T_4507, _T_4510) @[Rocket.scala 657:33]
    node _T_4513 = eq(_T_4497, SInt<2>("h-1")) @[Rocket.scala 657:61]
    node _T_4514 = bits(_T_4497, 0, 0) @[Rocket.scala 657:76]
    node _T_4515 = mux(_T_4511, _T_4513, _T_4514) @[Rocket.scala 657:10]
    node _T_4516 = mux(_T_4502, _T_4504, _T_4515) @[Rocket.scala 656:10]
    node _T_4517 = bits(alu.io_adder_out, 38, 0) @[Rocket.scala 658:16]
    skip
    skip
    skip
    node _T_4521 = and(mem_ctrl_mem, mem_xcpt) @[Rocket.scala 592:22]
    node _T_4523 = eq(io_dmem_s1_kill, UInt<1>("h0")) @[Rocket.scala 592:37]
    node _T_4524 = and(_T_4521, _T_4523) @[Rocket.scala 592:34]
    skip
    skip
    node _T_4527 = cat(cat(io_dmem_xcpt_ma_ld, io_dmem_xcpt_ma_st), cat(io_dmem_xcpt_pf_ld, io_dmem_xcpt_pf_st)) @[Rocket.scala 593:25]
    node _T_4529 = neq(_T_4527, UInt<4>("h0")) @[Rocket.scala 593:32]
    node _T_4530 = or(_T_4529, reset) @[Rocket.scala 593:11]
    node _T_4532 = eq(_T_4530, UInt<1>("h0")) @[Rocket.scala 593:11]
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
    node _T_4574 = bits(wb_reg_inst, 19, 15) @[Rocket.scala 599:58]
    node _T_4575 = bits(wb_reg_inst, 24, 20) @[Rocket.scala 599:58]
    skip
    node _T_4577 = bits(csr.io_time, 31, 0) @[Rocket.scala 637:32]
    node _T_4579 = mux(rf_wen, rf_waddr, UInt<5>("h0")) @[Rocket.scala 638:13]
    skip
    reg _T_4581 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_4581) @[Rocket.scala 639:42]
    reg _T_4582 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_4582) @[Rocket.scala 639:33]
    skip
    reg _T_4584 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_4584) @[Rocket.scala 640:42]
    reg _T_4585 : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_4585) @[Rocket.scala 640:33]
    node _T_4587 = eq(reset, UInt<1>("h0")) @[Rocket.scala 636:11]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    io_imem_req_valid <= or(take_pc_wb, take_pc_mem) @[Rocket.scala 537:21]
    io_imem_req_bits_pc <= mux(_T_4432, csr.io_evec, _T_4436) @[Rocket.scala 539:23]
    io_imem_req_bits_speculative <= eq(take_pc_wb, UInt<1>("h0")) @[Rocket.scala 538:32]
    io_imem_resp_ready <= ibuf.io_imem_ready @[Rocket.scala 168:16]
    io_imem_btb_update_valid <= or(_T_4445, _T_4459) @[Rocket.scala 549:28]
    io_imem_btb_update_bits_prediction_valid <= mem_reg_btb_hit @[Rocket.scala 556:44]
    io_imem_btb_update_bits_prediction_bits_taken <= mem_reg_btb_resp_taken @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_mask <= mem_reg_btb_resp_mask @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_bridx <= mem_reg_btb_resp_bridx @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_target <= mem_reg_btb_resp_target @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_entry <= mem_reg_btb_resp_entry @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_bht_history <= mem_reg_btb_resp_bht_history @[Rocket.scala 557:43]
    io_imem_btb_update_bits_prediction_bits_bht_value <= mem_reg_btb_resp_bht_value @[Rocket.scala 557:43]
    io_imem_btb_update_bits_pc <= not(_T_4478) @[Rocket.scala 555:30]
    io_imem_btb_update_bits_target <= bits(io_imem_req_bits_pc, 38, 0) @[Rocket.scala 553:34]
    io_imem_btb_update_bits_taken <= UInt<1>("h0")
    io_imem_btb_update_bits_isValid <= and(_T_4462, mem_cfi) @[Rocket.scala 550:35]
    io_imem_btb_update_bits_isJump <= or(mem_ctrl_jal, mem_ctrl_jalr) @[Rocket.scala 551:34]
    io_imem_btb_update_bits_isReturn <= and(mem_ctrl_jalr, _T_4469) @[Rocket.scala 552:36]
    io_imem_btb_update_bits_br_pc <= bits(_T_4475, 38, 0) @[Rocket.scala 554:33]
    io_imem_bht_update_valid <= and(_T_4448, mem_ctrl_branch) @[Rocket.scala 559:28]
    io_imem_bht_update_bits_prediction_valid <= io_imem_btb_update_bits_prediction_valid @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_taken <= io_imem_btb_update_bits_prediction_bits_taken @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_mask <= io_imem_btb_update_bits_prediction_bits_mask @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_bridx <= io_imem_btb_update_bits_prediction_bits_bridx @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_target <= io_imem_btb_update_bits_prediction_bits_target @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_entry <= io_imem_btb_update_bits_prediction_bits_entry @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_bht_history <= io_imem_btb_update_bits_prediction_bits_bht_history @[Rocket.scala 563:38]
    io_imem_bht_update_bits_prediction_bits_bht_value <= io_imem_btb_update_bits_prediction_bits_bht_value @[Rocket.scala 563:38]
    io_imem_bht_update_bits_pc <= io_imem_btb_update_bits_pc @[Rocket.scala 560:30]
    io_imem_bht_update_bits_taken <= bits(mem_reg_wdata, 0, 0) @[Rocket.scala 561:33]
    io_imem_bht_update_bits_mispredict <= mux(ex_pc_valid, _T_4030, _T_4033) @[Rocket.scala 562:38]
    io_imem_ras_update_valid <= and(mem_reg_valid, _T_4160) @[Rocket.scala 565:28]
    io_imem_ras_update_bits_isCall <= and(io_imem_btb_update_bits_isJump, _T_4487) @[Rocket.scala 567:34]
    io_imem_ras_update_bits_isReturn <= io_imem_btb_update_bits_isReturn @[Rocket.scala 568:36]
    io_imem_ras_update_bits_returnAddr <= bits(mem_int_wdata, 38, 0) @[Rocket.scala 566:38]
    io_imem_ras_update_bits_prediction_valid <= io_imem_btb_update_bits_prediction_valid @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_taken <= io_imem_btb_update_bits_prediction_bits_taken @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_mask <= io_imem_btb_update_bits_prediction_bits_mask @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_bridx <= io_imem_btb_update_bits_prediction_bits_bridx @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_target <= io_imem_btb_update_bits_prediction_bits_target @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_entry <= io_imem_btb_update_bits_prediction_bits_entry @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_bht_history <= io_imem_btb_update_bits_prediction_bits_bht_history @[Rocket.scala 569:38]
    io_imem_ras_update_bits_prediction_bits_bht_value <= io_imem_btb_update_bits_prediction_bits_bht_value @[Rocket.scala 569:38]
    io_imem_flush_icache <= and(_T_4438, _T_4440) @[Rocket.scala 544:24]
    io_imem_flush_tlb <= csr.io_fatc @[Rocket.scala 545:21]
    io_dmem_req_valid <= and(ex_reg_valid, ex_ctrl_mem) @[Rocket.scala 581:25]
    io_dmem_req_bits_addr <= cat(_T_4516, _T_4517) @[Rocket.scala 588:25]
    skip
    io_dmem_req_bits_tag <= pad(ex_dcache_tag, 7) @[Rocket.scala 584:25]
    io_dmem_req_bits_cmd <= ex_ctrl_mem_cmd @[Rocket.scala 585:25]
    io_dmem_req_bits_typ <= ex_ctrl_mem_type @[Rocket.scala 586:25]
    io_dmem_req_bits_phys <= UInt<1>("h0") @[Rocket.scala 587:25]
    io_dmem_req_bits_data <= UInt<64>("h0")
    io_dmem_s1_kill <= or(killm_common, mem_breakpoint) @[Rocket.scala 591:19]
    io_dmem_s1_data <= mux(mem_ctrl_fp, io_fpu_store_data, mem_reg_rs2) @[Rocket.scala 590:19]
    io_dmem_invalidate_lr <= wb_reg_xcpt @[Rocket.scala 589:25]
    io_ptw_ptbr_mode <= csr.io_ptbr_mode @[Rocket.scala 472:15]
    io_ptw_ptbr_asid <= UInt<16>("h0") @[Rocket.scala 472:15]
    io_ptw_ptbr_ppn <= csr.io_ptbr_ppn @[Rocket.scala 472:15]
    io_ptw_invalidate <= csr.io_fatc @[Rocket.scala 473:21]
    io_ptw_status_debug <= csr.io_status_debug @[Rocket.scala 474:17]
    io_ptw_status_isa <= csr.io_status_isa @[Rocket.scala 474:17]
    io_ptw_status_prv <= csr.io_status_prv @[Rocket.scala 474:17]
    io_ptw_status_sd <= csr.io_status_sd @[Rocket.scala 474:17]
    io_ptw_status_zero2 <= UInt<27>("h0") @[Rocket.scala 474:17]
    io_ptw_status_sxl <= UInt<2>("h2") @[Rocket.scala 474:17]
    io_ptw_status_uxl <= UInt<2>("h2") @[Rocket.scala 474:17]
    io_ptw_status_sd_rv32 <= UInt<1>("h0") @[Rocket.scala 474:17]
    io_ptw_status_zero1 <= UInt<8>("h0") @[Rocket.scala 474:17]
    io_ptw_status_tsr <= csr.io_status_tsr @[Rocket.scala 474:17]
    io_ptw_status_tw <= csr.io_status_tw @[Rocket.scala 474:17]
    io_ptw_status_tvm <= csr.io_status_tvm @[Rocket.scala 474:17]
    io_ptw_status_mxr <= csr.io_status_mxr @[Rocket.scala 474:17]
    io_ptw_status_pum <= csr.io_status_pum @[Rocket.scala 474:17]
    io_ptw_status_mprv <= csr.io_status_mprv @[Rocket.scala 474:17]
    io_ptw_status_xs <= UInt<2>("h0") @[Rocket.scala 474:17]
    io_ptw_status_fs <= csr.io_status_fs @[Rocket.scala 474:17]
    io_ptw_status_mpp <= csr.io_status_mpp @[Rocket.scala 474:17]
    io_ptw_status_hpp <= UInt<2>("h0") @[Rocket.scala 474:17]
    io_ptw_status_spp <= csr.io_status_spp @[Rocket.scala 474:17]
    io_ptw_status_mpie <= csr.io_status_mpie @[Rocket.scala 474:17]
    io_ptw_status_hpie <= UInt<1>("h0") @[Rocket.scala 474:17]
    io_ptw_status_spie <= csr.io_status_spie @[Rocket.scala 474:17]
    io_ptw_status_upie <= UInt<1>("h0") @[Rocket.scala 474:17]
    io_ptw_status_mie <= csr.io_status_mie @[Rocket.scala 474:17]
    io_ptw_status_hie <= UInt<1>("h0") @[Rocket.scala 474:17]
    io_ptw_status_sie <= csr.io_status_sie @[Rocket.scala 474:17]
    io_ptw_status_uie <= UInt<1>("h0") @[Rocket.scala 474:17]
    io_fpu_inst <= ibuf.io_inst_0_bits_inst_bits @[Rocket.scala 574:15]
    io_fpu_fromint_data <= mux(ex_reg_rs_bypass_0, _GEN_3, _T_3605) @[Rocket.scala 575:23]
    io_fpu_fcsr_rm <= csr.io_fcsr_rm @[Rocket.scala 467:18]
    io_fpu_dmem_resp_val <= and(dmem_resp_valid, _T_4176) @[Rocket.scala 576:24]
    io_fpu_dmem_resp_type <= io_dmem_resp_bits_typ @[Rocket.scala 578:25]
    io_fpu_dmem_resp_tag <= bits(io_dmem_resp_bits_tag, 5, 1) @[Rocket.scala 579:24]
    io_fpu_dmem_resp_data <= io_dmem_resp_bits_data_word_bypass @[Rocket.scala 577:25]
    io_fpu_valid <= and(_T_3444, id_ctrl_fp) @[Rocket.scala 571:16]
    io_fpu_killx <= or(_T_3792, _T_3794) @[Rocket.scala 572:16]
    io_fpu_killm <= or(_T_4150, _T_4152) @[Rocket.scala 573:16]
    io_rocc_cmd_valid <= UInt<1>("h0") @[Rocket.scala 596:21]
    io_rocc_cmd_bits_inst_funct <= bits(wb_reg_inst, 31, 25) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_rs2 <= bits(wb_reg_inst, 24, 20) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_rs1 <= bits(wb_reg_inst, 19, 15) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_xd <= bits(wb_reg_inst, 14, 14) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_xs1 <= bits(wb_reg_inst, 13, 13) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_xs2 <= bits(wb_reg_inst, 12, 12) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_rd <= bits(wb_reg_inst, 11, 7) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_inst_opcode <= bits(wb_reg_inst, 6, 0) @[Rocket.scala 599:25]
    io_rocc_cmd_bits_rs1 <= wb_reg_wdata @[Rocket.scala 600:24]
    io_rocc_cmd_bits_rs2 <= UInt<64>("h0") @[Rocket.scala 601:24]
    io_rocc_cmd_bits_status_debug <= csr.io_status_debug @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_isa <= csr.io_status_isa @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_prv <= csr.io_status_prv @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_sd <= csr.io_status_sd @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_zero2 <= UInt<27>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_sxl <= UInt<2>("h2") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_uxl <= UInt<2>("h2") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_sd_rv32 <= UInt<1>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_zero1 <= UInt<8>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_tsr <= csr.io_status_tsr @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_tw <= csr.io_status_tw @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_tvm <= csr.io_status_tvm @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_mxr <= csr.io_status_mxr @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_pum <= csr.io_status_pum @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_mprv <= csr.io_status_mprv @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_xs <= UInt<2>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_fs <= csr.io_status_fs @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_mpp <= csr.io_status_mpp @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_hpp <= UInt<2>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_spp <= csr.io_status_spp @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_mpie <= csr.io_status_mpie @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_hpie <= UInt<1>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_spie <= csr.io_status_spie @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_upie <= UInt<1>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_mie <= csr.io_status_mie @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_hie <= UInt<1>("h0") @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_sie <= csr.io_status_sie @[Rocket.scala 598:27]
    io_rocc_cmd_bits_status_uie <= UInt<1>("h0") @[Rocket.scala 598:27]
    io_rocc_resp_ready <= UInt<1>("h0")
    io_rocc_mem_req_ready <= UInt<1>("h0")
    io_rocc_mem_s2_nack <= UInt<1>("h0")
    io_rocc_mem_acquire <= UInt<1>("h0")
    io_rocc_mem_release <= UInt<1>("h0")
    io_rocc_mem_resp_valid <= UInt<1>("h0")
    io_rocc_mem_resp_bits_addr <= UInt<40>("h0")
    io_rocc_mem_resp_bits_tag <= UInt<7>("h0")
    io_rocc_mem_resp_bits_cmd <= UInt<5>("h0")
    io_rocc_mem_resp_bits_typ <= UInt<3>("h0")
    io_rocc_mem_resp_bits_data <= UInt<64>("h0")
    io_rocc_mem_resp_bits_replay <= UInt<1>("h0")
    io_rocc_mem_resp_bits_has_data <= UInt<1>("h0")
    io_rocc_mem_resp_bits_data_word_bypass <= UInt<64>("h0")
    io_rocc_mem_resp_bits_store_data <= UInt<64>("h0")
    io_rocc_mem_replay_next <= UInt<1>("h0")
    io_rocc_mem_xcpt_ma_ld <= UInt<1>("h0")
    io_rocc_mem_xcpt_ma_st <= UInt<1>("h0")
    io_rocc_mem_xcpt_pf_ld <= UInt<1>("h0")
    io_rocc_mem_xcpt_pf_st <= UInt<1>("h0")
    io_rocc_mem_ordered <= UInt<1>("h0")
    io_rocc_exception <= UInt<1>("h0") @[Rocket.scala 597:21]
    skip
    ex_ctrl_fp <= mux(_T_3444, id_ctrl_fp, ex_ctrl_fp) @[Rocket.scala 285:13]
    skip
    ex_ctrl_branch <= mux(_T_3444, id_ctrl_branch, ex_ctrl_branch) @[Rocket.scala 285:13]
    ex_ctrl_jal <= mux(_T_3444, id_ctrl_jal, ex_ctrl_jal) @[Rocket.scala 285:13]
    ex_ctrl_jalr <= mux(_T_3444, id_ctrl_jalr, ex_ctrl_jalr) @[Rocket.scala 285:13]
    ex_ctrl_rxs2 <= mux(_T_3444, id_ctrl_rxs2, ex_ctrl_rxs2) @[Rocket.scala 285:13]
    skip
    ex_ctrl_sel_alu2 <= mux(_T_3444, _GEN_21, ex_ctrl_sel_alu2) @[Rocket.scala 285:13 Rocket.scala 292:24 Rocket.scala 294:26]
    ex_ctrl_sel_alu1 <= mux(_T_3444, _GEN_20, ex_ctrl_sel_alu1) @[Rocket.scala 285:13 Rocket.scala 291:24]
    ex_ctrl_sel_imm <= mux(_T_3444, id_ctrl_sel_imm, ex_ctrl_sel_imm) @[Rocket.scala 285:13]
    ex_ctrl_alu_dw <= mux(_T_3444, _GEN_19, ex_ctrl_alu_dw) @[Rocket.scala 285:13 Rocket.scala 290:22]
    ex_ctrl_alu_fn <= mux(_T_3444, _GEN_18, ex_ctrl_alu_fn) @[Rocket.scala 285:13 Rocket.scala 289:22]
    ex_ctrl_mem <= mux(_T_3444, id_ctrl_mem, ex_ctrl_mem) @[Rocket.scala 285:13]
    ex_ctrl_mem_cmd <= mux(_T_3444, id_ctrl_mem_cmd, ex_ctrl_mem_cmd) @[Rocket.scala 285:13]
    ex_ctrl_mem_type <= mux(_T_3444, id_ctrl_mem_type, ex_ctrl_mem_type) @[Rocket.scala 285:13]
    skip
    skip
    skip
    ex_ctrl_wfd <= mux(_T_3444, id_ctrl_wfd, ex_ctrl_wfd) @[Rocket.scala 285:13]
    ex_ctrl_div <= mux(_T_3444, id_ctrl_div, ex_ctrl_div) @[Rocket.scala 285:13]
    ex_ctrl_wxd <= mux(_T_3444, id_ctrl_wxd, ex_ctrl_wxd) @[Rocket.scala 285:13]
    ex_ctrl_csr <= mux(_T_3444, id_csr, ex_ctrl_csr) @[Rocket.scala 285:13 Rocket.scala 287:17]
    ex_ctrl_fence_i <= mux(_T_3444, _GEN_24, ex_ctrl_fence_i) @[Rocket.scala 285:13 Rocket.scala 303:23]
    skip
    skip
    skip
    skip
    mem_ctrl_fp <= mux(ex_pc_valid, ex_ctrl_fp, mem_ctrl_fp) @[Rocket.scala 360:14]
    skip
    mem_ctrl_branch <= mux(ex_pc_valid, ex_ctrl_branch, mem_ctrl_branch) @[Rocket.scala 360:14]
    mem_ctrl_jal <= mux(ex_pc_valid, ex_ctrl_jal, mem_ctrl_jal) @[Rocket.scala 360:14]
    mem_ctrl_jalr <= mux(ex_pc_valid, ex_ctrl_jalr, mem_ctrl_jalr) @[Rocket.scala 360:14]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    mem_ctrl_mem <= mux(ex_pc_valid, ex_ctrl_mem, mem_ctrl_mem) @[Rocket.scala 360:14]
    skip
    skip
    skip
    skip
    skip
    mem_ctrl_wfd <= mux(ex_pc_valid, ex_ctrl_wfd, mem_ctrl_wfd) @[Rocket.scala 360:14]
    mem_ctrl_div <= mux(ex_pc_valid, ex_ctrl_div, mem_ctrl_div) @[Rocket.scala 360:14]
    mem_ctrl_wxd <= mux(ex_pc_valid, ex_ctrl_wxd, mem_ctrl_wxd) @[Rocket.scala 360:14]
    mem_ctrl_csr <= mux(ex_pc_valid, ex_ctrl_csr, mem_ctrl_csr) @[Rocket.scala 360:14]
    mem_ctrl_fence_i <= mux(ex_pc_valid, ex_ctrl_fence_i, mem_ctrl_fence_i) @[Rocket.scala 360:14]
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
    wb_ctrl_mem <= mux(mem_pc_valid, mem_ctrl_mem, wb_ctrl_mem) @[Rocket.scala 405:13]
    skip
    skip
    skip
    skip
    skip
    wb_ctrl_wfd <= mux(mem_pc_valid, mem_ctrl_wfd, wb_ctrl_wfd) @[Rocket.scala 405:13]
    wb_ctrl_div <= mux(mem_pc_valid, mem_ctrl_div, wb_ctrl_div) @[Rocket.scala 405:13]
    wb_ctrl_wxd <= mux(mem_pc_valid, mem_ctrl_wxd, wb_ctrl_wxd) @[Rocket.scala 405:13]
    wb_ctrl_csr <= mux(mem_pc_valid, mem_ctrl_csr, wb_ctrl_csr) @[Rocket.scala 405:13]
    wb_ctrl_fence_i <= mux(mem_pc_valid, mem_ctrl_fence_i, wb_ctrl_fence_i) @[Rocket.scala 405:13]
    skip
    skip
    skip
    ex_reg_xcpt_interrupt <= and(_T_3719, csr.io_interrupt) @[Rocket.scala 279:25]
    ex_reg_valid <= eq(ctrl_killd, UInt<1>("h0")) @[Rocket.scala 276:16]
    ex_reg_rvc <= mux(_T_3444, _GEN_22, ex_reg_rvc) @[Rocket.scala 286:16 Rocket.scala 295:20]
    ex_reg_btb_hit <= ibuf.io_inst_0_bits_btb_hit @[Rocket.scala 281:18]
    ex_reg_btb_resp_taken <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_taken, ex_reg_btb_resp_taken) @[Rocket.scala 282:57]
    ex_reg_btb_resp_mask <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_mask, ex_reg_btb_resp_mask) @[Rocket.scala 282:57]
    ex_reg_btb_resp_bridx <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_bridx, ex_reg_btb_resp_bridx) @[Rocket.scala 282:57]
    ex_reg_btb_resp_target <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_target, ex_reg_btb_resp_target) @[Rocket.scala 282:57]
    ex_reg_btb_resp_entry <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_entry, ex_reg_btb_resp_entry) @[Rocket.scala 282:57]
    ex_reg_btb_resp_bht_history <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_bht_history, ex_reg_btb_resp_bht_history) @[Rocket.scala 282:57]
    ex_reg_btb_resp_bht_value <= mux(ibuf.io_inst_0_bits_btb_hit, ibuf.io_btb_resp_bht_value, ex_reg_btb_resp_bht_value) @[Rocket.scala 282:57]
    ex_reg_xcpt <= and(_T_3444, id_xcpt) @[Rocket.scala 278:15]
    ex_reg_flush_pipe <= mux(_T_3444, _GEN_23, ex_reg_flush_pipe) @[Rocket.scala 298:23 Rocket.scala 302:25]
    ex_reg_load_use <= mux(_T_3444, id_load_use, ex_reg_load_use) @[Rocket.scala 299:21]
    ex_cause <= mux(id_xcpt, id_cause, ex_cause) @[Rocket.scala 280:33]
    ex_reg_replay <= and(_T_3719, ibuf.io_inst_0_bits_replay) @[Rocket.scala 277:17]
    ex_reg_pc <= mux(_T_3780, ibuf.io_pc, ex_reg_pc) @[Rocket.scala 319:15]
    ex_reg_inst <= mux(_T_3780, ibuf.io_inst_0_bits_inst_bits, ex_reg_inst) @[Rocket.scala 318:17]
    mem_reg_xcpt_interrupt <= and(_T_3718, ex_reg_xcpt_interrupt) @[Rocket.scala 356:26]
    mem_reg_valid <= eq(ctrl_killx, UInt<1>("h0")) @[Rocket.scala 353:17]
    mem_reg_rvc <= mux(ex_pc_valid, ex_reg_rvc, mem_reg_rvc) @[Rocket.scala 361:17]
    mem_reg_btb_hit <= mux(ex_pc_valid, ex_reg_btb_hit, mem_reg_btb_hit) @[Rocket.scala 364:21]
    mem_reg_btb_resp_taken <= mux(ex_pc_valid, _GEN_68, mem_reg_btb_resp_taken) @[Rocket.scala 365:46]
    mem_reg_btb_resp_mask <= mux(ex_pc_valid, _GEN_69, mem_reg_btb_resp_mask) @[Rocket.scala 365:46]
    mem_reg_btb_resp_bridx <= mux(ex_pc_valid, _GEN_70, mem_reg_btb_resp_bridx) @[Rocket.scala 365:46]
    mem_reg_btb_resp_target <= mux(ex_pc_valid, _GEN_71, mem_reg_btb_resp_target) @[Rocket.scala 365:46]
    mem_reg_btb_resp_entry <= mux(ex_pc_valid, _GEN_72, mem_reg_btb_resp_entry) @[Rocket.scala 365:46]
    mem_reg_btb_resp_bht_history <= mux(ex_pc_valid, _GEN_73, mem_reg_btb_resp_bht_history) @[Rocket.scala 365:46]
    mem_reg_btb_resp_bht_value <= mux(ex_pc_valid, _GEN_74, mem_reg_btb_resp_bht_value) @[Rocket.scala 365:46]
    mem_reg_xcpt <= and(_T_4054, ex_xcpt) @[Rocket.scala 355:16]
    mem_reg_replay <= and(_T_3718, replay_ex) @[Rocket.scala 354:18]
    mem_reg_flush_pipe <= mux(ex_pc_valid, ex_reg_flush_pipe, mem_reg_flush_pipe) @[Rocket.scala 366:24]
    mem_reg_cause <= mux(ex_xcpt, ex_cause, mem_reg_cause) @[Rocket.scala 357:34]
    mem_reg_slow_bypass <= mux(ex_pc_valid, ex_slow_bypass, mem_reg_slow_bypass) @[Rocket.scala 367:25]
    mem_reg_load <= mux(ex_pc_valid, _T_4077, mem_reg_load) @[Rocket.scala 362:18]
    mem_reg_store <= mux(ex_pc_valid, _T_4088, mem_reg_store) @[Rocket.scala 363:19]
    mem_reg_pc <= mux(ex_pc_valid, ex_reg_pc, mem_reg_pc) @[Rocket.scala 370:16]
    mem_reg_inst <= mux(ex_pc_valid, ex_reg_inst, mem_reg_inst) @[Rocket.scala 369:18]
    mem_reg_wdata <= mux(ex_pc_valid, alu.io_out, mem_reg_wdata) @[Rocket.scala 371:19]
    mem_reg_rs2 <= mux(ex_pc_valid, _GEN_75, mem_reg_rs2) @[Rocket.scala 373:19]
    wb_reg_valid <= eq(ctrl_killm, UInt<1>("h0")) @[Rocket.scala 400:16]
    wb_reg_xcpt <= and(mem_xcpt, _T_4160) @[Rocket.scala 402:15]
    wb_reg_replay <= and(replay_mem, _T_4160) @[Rocket.scala 401:17]
    wb_reg_cause <= mux(mem_xcpt, mem_cause, wb_reg_cause) @[Rocket.scala 403:34]
    wb_reg_pc <= mux(mem_pc_valid, mem_reg_pc, wb_reg_pc) @[Rocket.scala 411:15]
    wb_reg_inst <= mux(mem_pc_valid, mem_reg_inst, wb_reg_inst) @[Rocket.scala 410:17]
    wb_reg_wdata <= mux(mem_pc_valid, _T_4169, wb_reg_wdata) @[Rocket.scala 406:18]
    skip
    ibuf.clock <= clock
    ibuf.reset <= reset
    ibuf.io_imem_valid <= io_imem_resp_valid @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_valid <= io_imem_resp_bits_btb_valid @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_taken <= io_imem_resp_bits_btb_bits_taken @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_mask <= io_imem_resp_bits_btb_bits_mask @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_bridx <= io_imem_resp_bits_btb_bits_bridx @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_target <= io_imem_resp_bits_btb_bits_target @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_entry <= io_imem_resp_bits_btb_bits_entry @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_bht_history <= io_imem_resp_bits_btb_bits_bht_history @[Rocket.scala 168:16]
    ibuf.io_imem_bits_btb_bits_bht_value <= io_imem_resp_bits_btb_bits_bht_value @[Rocket.scala 168:16]
    ibuf.io_imem_bits_pc <= io_imem_resp_bits_pc @[Rocket.scala 168:16]
    ibuf.io_imem_bits_data <= io_imem_resp_bits_data @[Rocket.scala 168:16]
    skip
    ibuf.io_imem_bits_xcpt_if <= io_imem_resp_bits_xcpt_if @[Rocket.scala 168:16]
    ibuf.io_imem_bits_replay <= io_imem_resp_bits_replay @[Rocket.scala 168:16]
    ibuf.io_kill <= or(take_pc_wb, take_pc_mem) @[Rocket.scala 169:16]
    ibuf.io_inst_0_ready <= or(_T_4443, csr.io_interrupt) @[Rocket.scala 547:25]
    id_reg_fence <= mux(reset, UInt<1>("h0"), _T_3512) @[Rocket.scala 211:16]
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
    csr.clock <= clock
    csr.reset <= reset
    csr.io_interrupts_debug <= io_interrupts_debug @[Rocket.scala 465:21]
    csr.io_interrupts_mtip <= io_interrupts_mtip @[Rocket.scala 465:21]
    csr.io_interrupts_msip <= io_interrupts_msip @[Rocket.scala 465:21]
    csr.io_interrupts_meip <= io_interrupts_meip @[Rocket.scala 465:21]
    csr.io_interrupts_seip <= io_interrupts_seip @[Rocket.scala 465:21]
    csr.io_hartid <= io_hartid @[Rocket.scala 466:17]
    csr.io_rw_addr <= bits(wb_reg_inst, 31, 20) @[Rocket.scala 475:18]
    csr.io_rw_cmd <= mux(wb_reg_valid, wb_ctrl_csr, UInt<3>("h0")) @[Rocket.scala 476:17]
    csr.io_rw_wdata <= wb_reg_wdata @[Rocket.scala 477:19]
    csr.io_decode_csr <= bits(ibuf.io_inst_0_bits_raw, 31, 20) @[Rocket.scala 461:21]
    csr.io_exception <= wb_reg_xcpt @[Rocket.scala 462:20]
    csr.io_retire <= and(_T_4188, _T_4190) @[Rocket.scala 464:17]
    csr.io_cause <= wb_reg_cause @[Rocket.scala 463:16]
    csr.io_pc <= wb_reg_pc @[Rocket.scala 470:13]
    csr.io_badaddr <= cat(_T_4225, _T_4226) @[Rocket.scala 471:18]
    csr.io_fcsr_flags_valid <= io_fpu_fcsr_flags_valid @[Rocket.scala 468:21]
    csr.io_fcsr_flags_bits <= io_fpu_fcsr_flags_bits @[Rocket.scala 468:21]
    csr.io_rocc_interrupt <= io_rocc_interrupt @[Rocket.scala 469:25]
    skip
    skip
    bpu.io_status_debug <= csr.io_status_debug @[Rocket.scala 216:17]
    skip
    bpu.io_status_prv <= csr.io_status_prv @[Rocket.scala 216:17]
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
    bpu.io_bp_0_control_action <= csr.io_bp_0_control_action @[Rocket.scala 217:13]
    skip
    skip
    bpu.io_bp_0_control_tmatch <= csr.io_bp_0_control_tmatch @[Rocket.scala 217:13]
    bpu.io_bp_0_control_m <= csr.io_bp_0_control_m @[Rocket.scala 217:13]
    skip
    bpu.io_bp_0_control_s <= csr.io_bp_0_control_s @[Rocket.scala 217:13]
    bpu.io_bp_0_control_u <= csr.io_bp_0_control_u @[Rocket.scala 217:13]
    bpu.io_bp_0_control_x <= csr.io_bp_0_control_x @[Rocket.scala 217:13]
    bpu.io_bp_0_control_w <= csr.io_bp_0_control_w @[Rocket.scala 217:13]
    bpu.io_bp_0_control_r <= csr.io_bp_0_control_r @[Rocket.scala 217:13]
    bpu.io_bp_0_address <= csr.io_bp_0_address @[Rocket.scala 217:13]
    bpu.io_pc <= bits(ibuf.io_pc, 38, 0) @[Rocket.scala 218:13]
    bpu.io_ea <= bits(mem_reg_wdata, 38, 0) @[Rocket.scala 219:13]
    ex_reg_rs_bypass_0 <= mux(_T_3444, _T_3749, ex_reg_rs_bypass_0) @[Rocket.scala 309:27]
    ex_reg_rs_bypass_1 <= mux(_T_3444, _T_3764, ex_reg_rs_bypass_1) @[Rocket.scala 309:27]
    ex_reg_rs_lsb_0 <= mux(_T_3444, _GEN_25, ex_reg_rs_lsb_0) @[Rocket.scala 310:24 Rocket.scala 312:26]
    ex_reg_rs_lsb_1 <= mux(_T_3444, _GEN_27, ex_reg_rs_lsb_1) @[Rocket.scala 310:24 Rocket.scala 312:26]
    ex_reg_rs_msb_0 <= mux(_T_3444, _GEN_26, ex_reg_rs_msb_0) @[Rocket.scala 313:26]
    ex_reg_rs_msb_1 <= mux(_T_3444, _GEN_28, ex_reg_rs_msb_1) @[Rocket.scala 313:26]
    skip
    skip
    alu.io_dw <= ex_ctrl_alu_dw @[Rocket.scala 262:13]
    alu.io_fn <= ex_ctrl_alu_fn @[Rocket.scala 263:13]
    alu.io_in2 <= asUInt(ex_op2) @[Rocket.scala 264:14]
    alu.io_in1 <= asUInt(ex_op1) @[Rocket.scala 265:14]
    div.clock <= clock
    div.reset <= reset
    div.io_req_valid <= and(ex_reg_valid, ex_ctrl_div) @[Rocket.scala 269:20]
    div.io_req_bits_fn <= ex_ctrl_alu_fn @[Rocket.scala 271:22]
    div.io_req_bits_dw <= ex_ctrl_alu_dw @[Rocket.scala 270:22]
    div.io_req_bits_in1 <= mux(ex_reg_rs_bypass_0, _GEN_3, _T_3605) @[Rocket.scala 272:23]
    div.io_req_bits_in2 <= mux(ex_reg_rs_bypass_1, _GEN_7, _T_3607) @[Rocket.scala 273:23]
    div.io_req_bits_tag <= bits(ex_reg_inst, 11, 7) @[Rocket.scala 274:23]
    div.io_kill <= and(killm_common, _T_4154) @[Rocket.scala 396:15]
    div.io_resp_ready <= mux(_T_4183, UInt<1>("h0"), _T_4415) @[Rocket.scala 429:21 Rocket.scala 443:23]
    _T_4154 <= and(div.io_req_ready, div.io_req_valid) @[Rocket.scala 396:37]
    _T_4241 <= mux(reset, UInt<32>("h0"), _GEN_173) @[Rocket.scala 676:23 Rocket.scala 676:23]
    _T_4348 <= mux(reset, UInt<32>("h0"), _GEN_176) @[Rocket.scala 676:23 Rocket.scala 676:23 Rocket.scala 676:23]
    dcache_blocked <= and(_T_3785, _T_4395) @[Rocket.scala 523:18]
    skip
    _T_4581 <= mux(ex_reg_rs_bypass_0, _GEN_3, _T_3605) @[Rocket.scala 639:42]
    _T_4582 <= _T_4581 @[Rocket.scala 639:33]
    _T_4584 <= mux(ex_reg_rs_bypass_1, _GEN_7, _T_3607) @[Rocket.scala 640:42]
    _T_4585 <= _T_4584 @[Rocket.scala 640:33]
    printf(clock, and(_T_4524, _T_4532), "Assertion failed\n    at Rocket.scala:593 assert(io.dmem.xcpt.asUInt.orR) // make sure s1_kill is exhaustive\n") @[Rocket.scala 593:11]
    stop(clock, and(_T_4524, _T_4532), 1) @[Rocket.scala 593:11]
    printf(clock, _T_4587, "C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n", io_hartid, _T_4577, wb_valid, wb_reg_pc, _T_4579, rf_wdata, rf_wen, _T_4574, _T_4582, _T_4575, _T_4585, wb_reg_inst, wb_reg_inst) @[Rocket.scala 636:11]
    skip
    skip
    _T_3331._T_3340.clk <= clock
    _T_3331._T_3340.en <= UInt<1>("h1")
    _T_3331._T_3340.addr <= not(_T_3338)
    skip
    skip
    _T_3331._T_3350.clk <= clock
    _T_3331._T_3350.en <= UInt<1>("h1")
    _T_3331._T_3350.addr <= not(_T_3348)
    skip
    skip
    skip
    skip
    _T_3331._T_4200.clk <= clock
    _T_3331._T_4200.en <= and(rf_wen, _T_4197)
    _T_3331._T_4200.mask <= UInt<1>("h1")
    _T_3331._T_4200.addr <= not(rf_waddr)
    _T_3331._T_4200.data <= mux(_T_4191, io_dmem_resp_bits_data, _T_4195)

  module IBuf :
    input clock : Clock
    input reset : UInt<1>
    output io_imem_ready : UInt<1>
    input io_imem_valid : UInt<1>
    input io_imem_bits_btb_valid : UInt<1>
    input io_imem_bits_btb_bits_taken : UInt<1>
    input io_imem_bits_btb_bits_mask : UInt<2>
    input io_imem_bits_btb_bits_bridx : UInt<1>
    input io_imem_bits_btb_bits_target : UInt<39>
    input io_imem_bits_btb_bits_entry : UInt<6>
    input io_imem_bits_btb_bits_bht_history : UInt<7>
    input io_imem_bits_btb_bits_bht_value : UInt<2>
    input io_imem_bits_pc : UInt<40>
    input io_imem_bits_data : UInt<32>
    input io_imem_bits_xcpt_if : UInt<1>
    input io_imem_bits_replay : UInt<1>
    input io_kill : UInt<1>
    output io_pc : UInt<40>
    output io_btb_resp_taken : UInt<1>
    output io_btb_resp_mask : UInt<2>
    output io_btb_resp_bridx : UInt<1>
    output io_btb_resp_target : UInt<39>
    output io_btb_resp_entry : UInt<6>
    output io_btb_resp_bht_history : UInt<7>
    output io_btb_resp_bht_value : UInt<2>
    input io_inst_0_ready : UInt<1>
    output io_inst_0_valid : UInt<1>
    output io_inst_0_bits_pf0 : UInt<1>
    output io_inst_0_bits_pf1 : UInt<1>
    output io_inst_0_bits_replay : UInt<1>
    output io_inst_0_bits_btb_hit : UInt<1>
    output io_inst_0_bits_rvc : UInt<1>
    output io_inst_0_bits_inst_bits : UInt<32>
    output io_inst_0_bits_inst_rd : UInt<5>
    output io_inst_0_bits_inst_rs1 : UInt<5>
    output io_inst_0_bits_inst_rs2 : UInt<5>
    output io_inst_0_bits_inst_rs3 : UInt<5>
    output io_inst_0_bits_raw : UInt<32>
  
    inst RVCExpander of RVCExpander @[IBuf.scala 93:21]
    reg nBufValid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nBufValid) @[IBuf.scala 35:47]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    reg buf_pc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), buf_pc) @[IBuf.scala 36:16]
    reg buf_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), buf_data) @[IBuf.scala 36:16]
    skip
    reg buf_xcpt_if : UInt<1>, clock with :
      reset => (UInt<1>("h0"), buf_xcpt_if) @[IBuf.scala 36:16]
    reg buf_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), buf_replay) @[IBuf.scala 36:16]
    reg ibufBTBHit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ibufBTBHit) @[IBuf.scala 37:23]
    reg ibufBTBResp_taken : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_taken) @[IBuf.scala 38:24]
    reg ibufBTBResp_mask : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_mask) @[IBuf.scala 38:24]
    reg ibufBTBResp_bridx : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_bridx) @[IBuf.scala 38:24]
    reg ibufBTBResp_target : UInt<39>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_target) @[IBuf.scala 38:24]
    reg ibufBTBResp_entry : UInt<6>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_entry) @[IBuf.scala 38:24]
    reg ibufBTBResp_bht_history : UInt<7>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_bht_history) @[IBuf.scala 38:24]
    reg ibufBTBResp_bht_value : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ibufBTBResp_bht_value) @[IBuf.scala 38:24]
    node pcWordBits = bits(io_imem_bits_pc, 1, 1) @[Package.scala 44:13]
    node _T_375 = and(io_imem_bits_btb_valid, io_imem_bits_btb_bits_taken) @[IBuf.scala 43:40]
    node _T_377 = add(io_imem_bits_btb_bits_bridx, UInt<1>("h1")) @[IBuf.scala 43:100]
    node _T_379 = mux(_T_375, _T_377, UInt<2>("h2")) @[IBuf.scala 43:16]
    node _GEN_31 = pad(pcWordBits, 2) @[IBuf.scala 43:124]
    node _T_380 = sub(_T_379, _GEN_31) @[IBuf.scala 43:124]
    node _T_381 = asUInt(_T_380) @[IBuf.scala 43:124]
    node nIC = tail(_T_381, 1) @[IBuf.scala 43:124]
    node _T_565 = and(io_inst_0_ready, io_inst_0_valid) @[Decoupled.scala 30:37]
    skip
    skip
    skip
    skip
    node _T_572 = mux(RVCExpander.io_rvc, UInt<2>("h1"), UInt<2>("h2")) @[IBuf.scala 107:47]
    node nReady = mux(_T_565, _T_572, UInt<2>("h0")) @[IBuf.scala 107:32]
    skip
    node _GEN_32 = pad(nBufValid, 2) @[IBuf.scala 44:25]
    node _T_382 = sub(nReady, _GEN_32) @[IBuf.scala 44:25]
    node _T_383 = asUInt(_T_382) @[IBuf.scala 44:25]
    node nICReady = tail(_T_383, 1) @[IBuf.scala 44:25]
    node _T_385 = mux(io_imem_valid, nIC, UInt<2>("h0")) @[IBuf.scala 45:19]
    skip
    node _T_386 = add(_T_385, _GEN_32) @[IBuf.scala 45:49]
    node nValid = tail(_T_386, 1) @[IBuf.scala 45:49]
    skip
    node _T_387 = geq(nReady, _GEN_32) @[IBuf.scala 46:27]
    node _T_388 = geq(nICReady, nIC) @[IBuf.scala 46:53]
    node _T_390 = sub(nIC, nICReady) @[IBuf.scala 46:72]
    node _T_391 = asUInt(_T_390) @[IBuf.scala 46:72]
    node _T_392 = tail(_T_391, 1) @[IBuf.scala 46:72]
    node _T_393 = geq(UInt<2>("h1"), _T_392) @[IBuf.scala 46:65]
    node _T_394 = or(_T_388, _T_393) @[IBuf.scala 46:60]
    skip
    skip
    skip
    skip
    node _T_398 = sub(_GEN_32, nReady) @[IBuf.scala 49:62]
    node _T_399 = asUInt(_T_398) @[IBuf.scala 49:62]
    node _T_400 = tail(_T_399, 1) @[IBuf.scala 49:62]
    node _T_401 = mux(_T_387, UInt<2>("h0"), _T_400) @[IBuf.scala 49:21]
    skip
    skip
    node _T_403 = and(io_imem_valid, _T_387) @[IBuf.scala 56:25]
    node _T_404 = lt(nICReady, nIC) @[IBuf.scala 56:60]
    node _T_405 = and(_T_403, _T_404) @[IBuf.scala 56:48]
    skip
    skip
    skip
    skip
    node _T_411 = and(_T_405, _T_393) @[IBuf.scala 56:66]
    skip
    node _T_412 = add(_GEN_31, nICReady) @[IBuf.scala 57:30]
    node _T_413 = tail(_T_412, 1) @[IBuf.scala 57:30]
    skip
    skip
    skip
    node _T_417 = shr(io_imem_bits_data, 16) @[IBuf.scala 133:58]
    skip
    node _T_419 = cat(cat(_T_417, _T_417), io_imem_bits_data) @[Cat.scala 30:58]
    node _T_420 = shl(_T_413, 4) @[IBuf.scala 134:19]
    node _T_421 = dshr(_T_419, _T_420) @[IBuf.scala 134:10]
    node _T_422 = bits(_T_421, 15, 0) @[IBuf.scala 60:59]
    skip
    node _T_424 = and(io_imem_bits_pc, UInt<40>("hfffffffffc")) @[IBuf.scala 61:33]
    node _T_425 = shl(nICReady, 1) @[IBuf.scala 61:78]
    node _GEN_39 = pad(_T_425, 40) @[IBuf.scala 61:66]
    node _T_426 = add(io_imem_bits_pc, _GEN_39) @[IBuf.scala 61:66]
    node _T_427 = tail(_T_426, 1) @[IBuf.scala 61:66]
    node _T_428 = and(_T_427, UInt<40>("h3")) @[IBuf.scala 61:107]
    node _T_429 = or(_T_424, _T_428) @[IBuf.scala 61:47]
    node _GEN_40 = pad(io_imem_bits_btb_bits_bridx, 2) @[IBuf.scala 65:58]
    node _T_430 = add(_GEN_40, nICReady) @[IBuf.scala 65:58]
    node _T_431 = tail(_T_430, 1) @[IBuf.scala 65:58]
    node _GEN_0 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_taken, ibufBTBResp_taken) @[IBuf.scala 63:37]
    node _GEN_1 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_mask, ibufBTBResp_mask) @[IBuf.scala 63:37]
    node _GEN_2 = mux(io_imem_bits_btb_valid, _T_431, pad(ibufBTBResp_bridx, 2)) @[IBuf.scala 63:37]
    node _GEN_3 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_target, ibufBTBResp_target) @[IBuf.scala 63:37]
    node _GEN_4 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_entry, ibufBTBResp_entry) @[IBuf.scala 63:37]
    node _GEN_5 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_bht_history, ibufBTBResp_bht_history) @[IBuf.scala 63:37]
    node _GEN_6 = mux(io_imem_bits_btb_valid, io_imem_bits_btb_bits_bht_value, ibufBTBResp_bht_value) @[IBuf.scala 63:37]
    node _GEN_7 = mux(_T_411, _T_392, _T_401) @[IBuf.scala 56:90]
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
    node _GEN_24 = mux(_T_411, _GEN_2, pad(ibufBTBResp_bridx, 2)) @[IBuf.scala 56:90]
    skip
    skip
    skip
    skip
    node _GEN_29 = mux(io_kill, UInt<2>("h0"), _GEN_7) @[IBuf.scala 68:20]
    skip
    node _T_434 = add(UInt<2>("h2"), _GEN_32) @[IBuf.scala 73:32]
    node _T_435 = tail(_T_434, 1) @[IBuf.scala 73:32]
    skip
    node _T_436 = sub(_T_435, _GEN_31) @[IBuf.scala 73:44]
    node _T_437 = asUInt(_T_436) @[IBuf.scala 73:44]
    node icShiftAmt = tail(_T_437, 1) @[IBuf.scala 73:44]
    skip
    node _T_439 = bits(io_imem_bits_data, 15, 0) @[IBuf.scala 74:87]
    skip
    node _T_441 = cat(io_imem_bits_data, cat(_T_439, _T_439)) @[Cat.scala 30:58]
    node _T_442 = shr(_T_441, 48) @[IBuf.scala 126:58]
    skip
    skip
    node _T_445 = cat(cat(cat(_T_442, _T_442), cat(_T_442, _T_442)), cat(io_imem_bits_data, cat(_T_439, _T_439))) @[Cat.scala 30:58]
    node _T_446 = shl(icShiftAmt, 4) @[IBuf.scala 127:19]
    node _GEN_43 = pad(_T_445, 191) @[IBuf.scala 127:10]
    node _T_447 = dshlw(_GEN_43, _T_446) @[IBuf.scala 127:10]
    node icData = bits(_T_447, 95, 64) @[Package.scala 44:13]
    skip
    node _T_450 = shl(nBufValid, 4) @[IBuf.scala 76:65]
    node _T_451 = dshlw(UInt<63>("hffffffff"), _T_450) @[IBuf.scala 76:51]
    node icMask = bits(_T_451, 31, 0) @[IBuf.scala 76:92]
    node _T_452 = and(icData, icMask) @[IBuf.scala 77:21]
    node _T_453 = not(icMask) @[IBuf.scala 77:43]
    node _T_454 = and(buf_data, _T_453) @[IBuf.scala 77:41]
    skip
    node _T_456 = dshlw(UInt<4>("h1"), nValid) @[OneHot.scala 47:11]
    node _T_458 = sub(_T_456, UInt<4>("h1")) @[IBuf.scala 79:33]
    node _T_459 = asUInt(_T_458) @[IBuf.scala 79:33]
    node _T_460 = tail(_T_459, 1) @[IBuf.scala 79:33]
    node valid = bits(_T_460, 1, 0) @[IBuf.scala 79:37]
    node _T_462 = dshlw(UInt<2>("h1"), nBufValid) @[OneHot.scala 47:11]
    node _T_464 = sub(_T_462, UInt<2>("h1")) @[IBuf.scala 80:37]
    node _T_465 = asUInt(_T_464) @[IBuf.scala 80:37]
    node bufMask = tail(_T_465, 1) @[IBuf.scala 80:37]
    node _T_467 = mux(buf_xcpt_if, bufMask, UInt<2>("h0")) @[IBuf.scala 81:29]
    node _T_468 = not(bufMask) @[IBuf.scala 81:89]
    node _T_470 = mux(io_imem_bits_xcpt_if, _T_468, UInt<2>("h0")) @[IBuf.scala 81:66]
    node _T_471 = or(_T_467, _T_470) @[IBuf.scala 81:61]
    node xcpt_if = and(valid, _T_471) @[IBuf.scala 81:23]
    node _T_473 = mux(buf_replay, bufMask, UInt<2>("h0")) @[IBuf.scala 82:31]
    skip
    node _T_476 = mux(io_imem_bits_replay, _T_468, UInt<2>("h0")) @[IBuf.scala 82:67]
    node _T_477 = or(_T_473, _T_476) @[IBuf.scala 82:62]
    node ic_replay = and(valid, _T_477) @[IBuf.scala 82:25]
    node _T_479 = dshlw(UInt<2>("h1"), ibufBTBResp_bridx) @[OneHot.scala 47:11]
    node ibufBTBHitMask = mux(ibufBTBHit, _T_479, UInt<2>("h0")) @[IBuf.scala 83:27]
    node _T_482 = eq(io_imem_bits_btb_valid, UInt<1>("h0")) @[IBuf.scala 84:10]
    node _T_483 = geq(io_imem_bits_btb_bits_bridx, pcWordBits) @[IBuf.scala 84:65]
    node _T_484 = or(_T_482, _T_483) @[IBuf.scala 84:34]
    node _T_485 = or(_T_484, reset) @[IBuf.scala 84:9]
    node _T_487 = eq(_T_485, UInt<1>("h0")) @[IBuf.scala 84:9]
    node _T_488 = add(io_imem_bits_btb_bits_bridx, nBufValid) @[IBuf.scala 85:87]
    skip
    node _T_489 = sub(_T_488, _GEN_31) @[IBuf.scala 85:100]
    node _T_490 = asUInt(_T_489) @[IBuf.scala 85:100]
    node _T_491 = tail(_T_490, 1) @[IBuf.scala 85:100]
    node _T_493 = dshlw(UInt<4>("h1"), _T_491) @[OneHot.scala 47:11]
    node icBTBHitMask = mux(io_imem_bits_btb_valid, _T_493, UInt<4>("h0")) @[IBuf.scala 85:25]
    node _T_495 = and(ibufBTBHitMask, bufMask) @[IBuf.scala 86:35]
    skip
    node _GEN_45 = pad(_T_468, 4) @[IBuf.scala 86:60]
    node _T_497 = and(icBTBHitMask, _GEN_45) @[IBuf.scala 86:60]
    node _GEN_46 = pad(_T_495, 4) @[IBuf.scala 86:45]
    node btbHitMask = or(_GEN_46, _T_497) @[IBuf.scala 86:45]
    skip
    node _T_500 = neq(_T_495, UInt<2>("h0")) @[IBuf.scala 88:49]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_511 = gt(nBufValid, UInt<1>("h0")) @[IBuf.scala 89:26]
    skip
    skip
    node _T_515 = bits(ic_replay, 0, 0) @[IBuf.scala 99:29]
    node _T_517 = eq(RVCExpander.io_rvc, UInt<1>("h0")) @[IBuf.scala 99:37]
    skip
    node _T_519 = bits(btbHitMask, 0, 0) @[IBuf.scala 99:63]
    skip
    skip
    node _GEN_47 = shr(ic_replay, 1) @[IBuf.scala 99:79]
    node _T_523 = pad(_GEN_47, 2) @[IBuf.scala 99:79]
    node _T_524 = bits(_T_523, 0, 0) @[IBuf.scala 99:79]
    node _T_525 = or(_T_519, _T_524) @[IBuf.scala 99:67]
    node _T_526 = and(_T_517, _T_525) @[IBuf.scala 99:49]
    node _T_527 = or(_T_515, _T_526) @[IBuf.scala 99:33]
    skip
    node _T_529 = bits(valid, 0, 0) @[IBuf.scala 100:32]
    skip
    skip
    node _GEN_48 = shr(valid, 1) @[IBuf.scala 100:59]
    node _T_533 = pad(_GEN_48, 2) @[IBuf.scala 100:59]
    node _T_534 = bits(_T_533, 0, 0) @[IBuf.scala 100:59]
    node _T_535 = or(RVCExpander.io_rvc, _T_534) @[IBuf.scala 100:51]
    skip
    skip
    node _GEN_49 = shr(xcpt_if, 1) @[IBuf.scala 100:75]
    node _T_539 = pad(_GEN_49, 2) @[IBuf.scala 100:75]
    node _T_540 = bits(_T_539, 0, 0) @[IBuf.scala 100:75]
    node _T_541 = or(_T_535, _T_540) @[IBuf.scala 100:65]
    node _T_542 = or(_T_541, _T_527) @[IBuf.scala 100:81]
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
    node _GEN_51 = shr(btbHitMask, 1) @[IBuf.scala 104:77]
    node _T_561 = pad(_GEN_51, 4) @[IBuf.scala 104:77]
    node _T_562 = bits(_T_561, 0, 0) @[IBuf.scala 104:77]
    node _T_563 = and(_T_517, _T_562) @[IBuf.scala 104:64]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    io_imem_ready <= and(_T_387, _T_394) @[IBuf.scala 46:17]
    io_pc <= mux(_T_511, buf_pc, io_imem_bits_pc) @[IBuf.scala 89:9]
    io_btb_resp_taken <= mux(_T_500, ibufBTBResp_taken, io_imem_bits_btb_bits_taken) @[IBuf.scala 88:15]
    io_btb_resp_mask <= mux(_T_500, ibufBTBResp_mask, io_imem_bits_btb_bits_mask) @[IBuf.scala 88:15]
    io_btb_resp_bridx <= mux(_T_500, ibufBTBResp_bridx, io_imem_bits_btb_bits_bridx) @[IBuf.scala 88:15]
    io_btb_resp_target <= mux(_T_500, ibufBTBResp_target, io_imem_bits_btb_bits_target) @[IBuf.scala 88:15]
    io_btb_resp_entry <= mux(_T_500, ibufBTBResp_entry, io_imem_bits_btb_bits_entry) @[IBuf.scala 88:15]
    io_btb_resp_bht_history <= mux(_T_500, ibufBTBResp_bht_history, io_imem_bits_btb_bits_bht_history) @[IBuf.scala 88:15]
    io_btb_resp_bht_value <= mux(_T_500, ibufBTBResp_bht_value, io_imem_bits_btb_bits_bht_value) @[IBuf.scala 88:15]
    io_inst_0_valid <= and(_T_529, _T_542) @[IBuf.scala 100:24]
    io_inst_0_bits_pf0 <= bits(xcpt_if, 0, 0) @[IBuf.scala 101:27]
    io_inst_0_bits_pf1 <= and(_T_517, _T_540) @[IBuf.scala 102:27]
    io_inst_0_bits_replay <= or(_T_515, _T_526) @[IBuf.scala 103:30]
    io_inst_0_bits_btb_hit <= or(_T_519, _T_563) @[IBuf.scala 104:31]
    io_inst_0_bits_rvc <= RVCExpander.io_rvc @[IBuf.scala 105:27]
    io_inst_0_bits_inst_bits <= RVCExpander.io_out_bits @[IBuf.scala 95:26]
    io_inst_0_bits_inst_rd <= RVCExpander.io_out_rd @[IBuf.scala 95:26]
    io_inst_0_bits_inst_rs1 <= RVCExpander.io_out_rs1 @[IBuf.scala 95:26]
    io_inst_0_bits_inst_rs2 <= RVCExpander.io_out_rs2 @[IBuf.scala 95:26]
    io_inst_0_bits_inst_rs3 <= RVCExpander.io_out_rs3 @[IBuf.scala 95:26]
    io_inst_0_bits_raw <= or(_T_452, _T_454) @[IBuf.scala 96:25]
    nBufValid <= mux(reset, UInt<1>("h0"), bits(_GEN_29, 0, 0)) @[IBuf.scala 49:15 IBuf.scala 58:17 IBuf.scala 69:17]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    buf_pc <= mux(_T_411, _T_429, buf_pc) @[IBuf.scala 59:11 IBuf.scala 61:14]
    buf_data <= mux(_T_411, pad(_T_422, 32), buf_data) @[IBuf.scala 59:11 IBuf.scala 60:16]
    skip
    buf_xcpt_if <= mux(_T_411, io_imem_bits_xcpt_if, buf_xcpt_if) @[IBuf.scala 59:11]
    buf_replay <= mux(_T_411, io_imem_bits_replay, buf_replay) @[IBuf.scala 59:11]
    ibufBTBHit <= mux(_T_411, io_imem_bits_btb_valid, ibufBTBHit) @[IBuf.scala 62:18]
    ibufBTBResp_taken <= mux(_T_411, _GEN_0, ibufBTBResp_taken) @[IBuf.scala 64:21]
    ibufBTBResp_mask <= mux(_T_411, _GEN_1, ibufBTBResp_mask) @[IBuf.scala 64:21]
    ibufBTBResp_bridx <= bits(_GEN_24, 0, 0) @[IBuf.scala 64:21 IBuf.scala 65:27]
    ibufBTBResp_target <= mux(_T_411, _GEN_3, ibufBTBResp_target) @[IBuf.scala 64:21]
    ibufBTBResp_entry <= mux(_T_411, _GEN_4, ibufBTBResp_entry) @[IBuf.scala 64:21]
    ibufBTBResp_bht_history <= mux(_T_411, _GEN_5, ibufBTBResp_bht_history) @[IBuf.scala 64:21]
    ibufBTBResp_bht_value <= mux(_T_411, _GEN_6, ibufBTBResp_bht_value) @[IBuf.scala 64:21]
    skip
    skip
    RVCExpander.io_in <= or(_T_452, _T_454) @[IBuf.scala 94:15]
    printf(clock, _T_487, "Assertion failed\n    at IBuf.scala:84 assert(!io.imem.bits.btb.valid || io.imem.bits.btb.bits.bridx >= pcWordBits)\n") @[IBuf.scala 84:9]
    stop(clock, _T_487, 1) @[IBuf.scala 84:9]

  module CSRFile :
    input clock : Clock
    input reset : UInt<1>
    input io_interrupts_debug : UInt<1>
    input io_interrupts_mtip : UInt<1>
    input io_interrupts_msip : UInt<1>
    input io_interrupts_meip : UInt<1>
    input io_interrupts_seip : UInt<1>
    input io_hartid : UInt<64>
    input io_rw_addr : UInt<12>
    input io_rw_cmd : UInt<3>
    output io_rw_rdata : UInt<64>
    input io_rw_wdata : UInt<64>
    input io_decode_csr : UInt<12>
    output io_decode_fp_illegal : UInt<1>
    output io_decode_read_illegal : UInt<1>
    output io_decode_write_illegal : UInt<1>
    output io_decode_write_flush : UInt<1>
    output io_decode_system_illegal : UInt<1>
    output io_csr_stall : UInt<1>
    output io_eret : UInt<1>
    output io_singleStep : UInt<1>
    output io_status_debug : UInt<1>
    output io_status_isa : UInt<32>
    output io_status_prv : UInt<2>
    output io_status_sd : UInt<1>
    output io_status_zero2 : UInt<27>
    output io_status_sxl : UInt<2>
    output io_status_uxl : UInt<2>
    output io_status_sd_rv32 : UInt<1>
    output io_status_zero1 : UInt<8>
    output io_status_tsr : UInt<1>
    output io_status_tw : UInt<1>
    output io_status_tvm : UInt<1>
    output io_status_mxr : UInt<1>
    output io_status_pum : UInt<1>
    output io_status_mprv : UInt<1>
    output io_status_xs : UInt<2>
    output io_status_fs : UInt<2>
    output io_status_mpp : UInt<2>
    output io_status_hpp : UInt<2>
    output io_status_spp : UInt<1>
    output io_status_mpie : UInt<1>
    output io_status_hpie : UInt<1>
    output io_status_spie : UInt<1>
    output io_status_upie : UInt<1>
    output io_status_mie : UInt<1>
    output io_status_hie : UInt<1>
    output io_status_sie : UInt<1>
    output io_status_uie : UInt<1>
    output io_ptbr_mode : UInt<4>
    output io_ptbr_ppn : UInt<44>
    output io_evec : UInt<40>
    input io_exception : UInt<1>
    input io_retire : UInt<1>
    input io_cause : UInt<64>
    input io_pc : UInt<40>
    input io_badaddr : UInt<40>
    output io_fatc : UInt<1>
    output io_time : UInt<64>
    output io_fcsr_rm : UInt<3>
    input io_fcsr_flags_valid : UInt<1>
    input io_fcsr_flags_bits : UInt<5>
    input io_rocc_interrupt : UInt<1>
    output io_interrupt : UInt<1>
    output io_interrupt_cause : UInt<64>
    output io_bp_0_control_action : UInt<1>
    output io_bp_0_control_tmatch : UInt<2>
    output io_bp_0_control_m : UInt<1>
    output io_bp_0_control_s : UInt<1>
    output io_bp_0_control_u : UInt<1>
    output io_bp_0_control_x : UInt<1>
    output io_bp_0_control_w : UInt<1>
    output io_bp_0_control_r : UInt<1>
    output io_bp_0_address : UInt<39>
  
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
    reg reg_mstatus_prv : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_prv) @[CSR.scala 197:24]
    skip
    skip
    skip
    skip
    skip
    skip
    reg reg_mstatus_tsr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_tsr) @[CSR.scala 197:24]
    reg reg_mstatus_tw : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_tw) @[CSR.scala 197:24]
    reg reg_mstatus_tvm : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_tvm) @[CSR.scala 197:24]
    reg reg_mstatus_mxr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mxr) @[CSR.scala 197:24]
    reg reg_mstatus_pum : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_pum) @[CSR.scala 197:24]
    reg reg_mstatus_mprv : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mprv) @[CSR.scala 197:24]
    skip
    reg reg_mstatus_fs : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_fs) @[CSR.scala 197:24]
    reg reg_mstatus_mpp : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mpp) @[CSR.scala 197:24]
    skip
    reg reg_mstatus_spp : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_spp) @[CSR.scala 197:24]
    reg reg_mstatus_mpie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mpie) @[CSR.scala 197:24]
    skip
    reg reg_mstatus_spie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_spie) @[CSR.scala 197:24]
    skip
    reg reg_mstatus_mie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mie) @[CSR.scala 197:24]
    skip
    reg reg_mstatus_sie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_sie) @[CSR.scala 197:24]
    skip
    node system_insn = eq(io_rw_cmd, UInt<3>("h4")) @[CSR.scala 409:31]
    node _T_1733 = bits(io_rw_addr, 2, 0) @[CSR.scala 410:37]
    node opcode = dshlw(UInt<8>("h1"), _T_1733) @[CSR.scala 410:24]
    node _T_1739 = bits(opcode, 2, 2) @[CSR.scala 414:39]
    node insn_ret = and(system_insn, _T_1739) @[CSR.scala 414:30]
    node _T_2450 = bits(io_rw_addr, 9, 9) @[CSR.scala 506:39]
    node _T_2452 = eq(_T_2450, UInt<1>("h0")) @[CSR.scala 506:28]
    skip
    node _T_2465 = eq(_T_2452, UInt<1>("h0")) @[CSR.scala 506:44]
    node _T_2458 = bits(io_rw_addr, 10, 10) @[CSR.scala 512:47]
    skip
    node _T_2467 = eq(_T_2458, UInt<1>("h0")) @[CSR.scala 512:53]
    node _T_2468 = and(_T_2465, _T_2467) @[CSR.scala 512:53]
    skip
    node _T_2462 = and(_T_2465, _T_2458) @[CSR.scala 512:53]
    reg reg_dcsr_prv : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_prv) @[CSR.scala 205:21]
    node insn_rs2 = bits(io_rw_addr, 5, 5) @[CSR.scala 411:28]
    node _T_1735 = eq(insn_rs2, UInt<1>("h0")) @[CSR.scala 412:34]
    node _T_1736 = and(system_insn, _T_1735) @[CSR.scala 412:31]
    node _T_1737 = bits(opcode, 0, 0) @[CSR.scala 412:53]
    node insn_call = and(_T_1736, _T_1737) @[CSR.scala 412:44]
    node _T_1738 = bits(opcode, 1, 1) @[CSR.scala 413:41]
    node insn_break = and(system_insn, _T_1738) @[CSR.scala 413:32]
    node _T_2344 = or(insn_call, insn_break) @[CSR.scala 461:29]
    node exception = or(_T_2344, io_exception) @[CSR.scala 461:43]
    reg reg_singleStepped : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_singleStepped) @[CSR.scala 236:30]
    node _GEN_354 = pad(reg_mstatus_prv, 4) @[CSR.scala 437:36]
    node _T_2247 = add(_GEN_354, UInt<4>("h8")) @[CSR.scala 437:36]
    node _T_2248 = tail(_T_2247, 1) @[CSR.scala 437:36]
    node _T_2250 = mux(insn_break, UInt<64>("h3"), io_cause) @[CSR.scala 438:14]
    node cause = mux(insn_call, pad(_T_2248, 64), _T_2250) @[CSR.scala 437:8]
    node _T_2251 = bits(cause, 63, 63) @[CSR.scala 440:30]
    node cause_lsbs = bits(cause, 5, 0) @[CSR.scala 439:25]
    node _T_2267 = eq(cause_lsbs, UInt<6>("hd")) @[CSR.scala 440:53]
    node causeIsDebugInt = and(_T_2251, _T_2267) @[CSR.scala 440:39]
    node _T_2311 = or(reg_singleStepped, causeIsDebugInt) @[CSR.scala 443:60]
    skip
    node _T_2270 = eq(_T_2251, UInt<1>("h0")) @[CSR.scala 441:29]
    skip
    node causeIsDebugTrigger = and(_T_2270, _T_2267) @[CSR.scala 441:44]
    node _T_2312 = or(_T_2311, causeIsDebugTrigger) @[CSR.scala 443:79]
    skip
    skip
    node _T_2304 = and(_T_2270, insn_break) @[CSR.scala 442:42]
    reg reg_dcsr_ebreakm : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_ebreakm) @[CSR.scala 205:21]
    skip
    skip
    reg reg_dcsr_ebreaks : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_ebreaks) @[CSR.scala 205:21]
    reg reg_dcsr_ebreaku : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_ebreaku) @[CSR.scala 205:21]
    skip
    node _T_2307 = cat(cat(reg_dcsr_ebreakm, UInt<1>("h0")), cat(reg_dcsr_ebreaks, reg_dcsr_ebreaku)) @[Cat.scala 30:58]
    node _T_2308 = dshr(_T_2307, reg_mstatus_prv) @[CSR.scala 442:134]
    node _T_2309 = bits(_T_2308, 0, 0) @[CSR.scala 442:134]
    node causeIsDebugBreak = and(_T_2304, _T_2309) @[CSR.scala 442:56]
    node _T_2313 = or(_T_2312, causeIsDebugBreak) @[CSR.scala 443:102]
    reg reg_debug : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_debug) @[CSR.scala 232:22]
    node trapToDebug = or(_T_2313, reg_debug) @[CSR.scala 443:123]
    skip
    node _T_2435 = eq(trapToDebug, UInt<1>("h0")) @[CSR.scala 481:24]
    node _T_2317 = leq(reg_mstatus_prv, UInt<2>("h1")) @[CSR.scala 444:51]
    skip
    skip
    reg reg_mideleg : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_mideleg) @[CSR.scala 242:24]
    node _T_2320 = dshr(reg_mideleg, cause_lsbs) @[CSR.scala 444:93]
    node _T_2321 = bits(_T_2320, 0, 0) @[CSR.scala 444:93]
    reg reg_medeleg : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_medeleg) @[CSR.scala 243:24]
    node _T_2322 = dshr(reg_medeleg, cause_lsbs) @[CSR.scala 444:118]
    node _T_2323 = bits(_T_2322, 0, 0) @[CSR.scala 444:118]
    node _T_2324 = mux(_T_2251, _T_2321, _T_2323) @[CSR.scala 444:66]
    node delegate = and(_T_2317, _T_2324) @[CSR.scala 444:60]
    node _T_2437 = eq(delegate, UInt<1>("h0")) @[CSR.scala 486:27]
    node _T_2438 = and(_T_2435, _T_2437) @[CSR.scala 486:27]
    skip
    node _T_2423 = and(_T_2435, delegate) @[CSR.scala 486:27]
    node _GEN_51 = mux(_T_2423, UInt<2>("h1"), reg_mstatus_prv) @[CSR.scala 486:27]
    node _GEN_59 = mux(_T_2438, UInt<2>("h3"), _GEN_51) @[CSR.scala 494:17]
    node _GEN_70 = mux(exception, _GEN_59, reg_mstatus_prv) @[CSR.scala 473:20]
    node _GEN_81 = mux(_T_2452, pad(reg_mstatus_spp, 2), _GEN_70) @[CSR.scala 506:44]
    node _GEN_83 = mux(_T_2462, reg_dcsr_prv, _GEN_81) @[CSR.scala 512:53]
    node _GEN_92 = mux(_T_2468, reg_mstatus_mpp, _GEN_83) @[CSR.scala 516:17]
    node new_prv = mux(insn_ret, _GEN_92, _GEN_70) @[CSR.scala 505:19]
    skip
    node _T_465 = eq(new_prv, UInt<2>("h2")) @[CSR.scala 697:27]
    node _T_467 = mux(_T_465, UInt<2>("h0"), new_prv) @[CSR.scala 697:21]
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
    reg reg_dcsr_cause : UInt<3>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_cause) @[CSR.scala 205:21]
    reg reg_dcsr_debugint : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_debugint) @[CSR.scala 205:21]
    skip
    reg reg_dcsr_halt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_halt) @[CSR.scala 205:21]
    reg reg_dcsr_step : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_step) @[CSR.scala 205:21]
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
    node effective_prv = cat(reg_debug, reg_mstatus_prv) @[Cat.scala 30:58]
    reg reg_dpc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), reg_dpc) @[CSR.scala 234:20]
    reg reg_dscratch : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_dscratch) @[CSR.scala 235:25]
    skip
    skip
    reg reg_bp_0_control_dmode : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_dmode) @[CSR.scala 239:19]
    skip
    skip
    reg reg_bp_0_control_action : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_action) @[CSR.scala 239:19]
    skip
    skip
    reg reg_bp_0_control_tmatch : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_tmatch) @[CSR.scala 239:19]
    reg reg_bp_0_control_m : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_m) @[CSR.scala 239:19]
    skip
    reg reg_bp_0_control_s : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_s) @[CSR.scala 239:19]
    reg reg_bp_0_control_u : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_u) @[CSR.scala 239:19]
    reg reg_bp_0_control_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_x) @[CSR.scala 239:19]
    reg reg_bp_0_control_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_w) @[CSR.scala 239:19]
    reg reg_bp_0_control_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_r) @[CSR.scala 239:19]
    reg reg_bp_0_address : UInt<39>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_address) @[CSR.scala 239:19]
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
    reg reg_mie : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_mie) @[CSR.scala 241:20]
    skip
    reg reg_mip_meip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_meip) @[CSR.scala 244:20]
    skip
    reg reg_mip_seip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_seip) @[CSR.scala 244:20]
    skip
    reg reg_mip_mtip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_mtip) @[CSR.scala 244:20]
    skip
    reg reg_mip_stip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_stip) @[CSR.scala 244:20]
    skip
    reg reg_mip_msip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_msip) @[CSR.scala 244:20]
    skip
    reg reg_mip_ssip : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mip_ssip) @[CSR.scala 244:20]
    skip
    reg reg_mepc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), reg_mepc) @[CSR.scala 245:21]
    reg reg_mcause : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_mcause) @[CSR.scala 246:23]
    reg reg_mbadaddr : UInt<40>, clock with :
      reset => (UInt<1>("h0"), reg_mbadaddr) @[CSR.scala 247:25]
    reg reg_mscratch : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_mscratch) @[CSR.scala 248:25]
    reg reg_mtvec : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mtvec) @[CSR.scala 251:27]
    reg reg_mcounteren : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mcounteren) @[CSR.scala 254:27]
    reg reg_scounteren : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_scounteren) @[CSR.scala 255:27]
    reg reg_sepc : UInt<40>, clock with :
      reset => (UInt<1>("h0"), reg_sepc) @[CSR.scala 258:21]
    reg reg_scause : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_scause) @[CSR.scala 259:23]
    reg reg_sbadaddr : UInt<40>, clock with :
      reset => (UInt<1>("h0"), reg_sbadaddr) @[CSR.scala 260:25]
    reg reg_sscratch : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_sscratch) @[CSR.scala 261:25]
    reg reg_stvec : UInt<39>, clock with :
      reset => (UInt<1>("h0"), reg_stvec) @[CSR.scala 262:22]
    reg reg_sptbr_mode : UInt<4>, clock with :
      reset => (UInt<1>("h0"), reg_sptbr_mode) @[CSR.scala 263:22]
    skip
    reg reg_sptbr_ppn : UInt<44>, clock with :
      reset => (UInt<1>("h0"), reg_sptbr_ppn) @[CSR.scala 263:22]
    reg reg_wfi : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_wfi) @[CSR.scala 264:20]
    reg reg_fflags : UInt<5>, clock with :
      reset => (UInt<1>("h0"), reg_fflags) @[CSR.scala 266:23]
    reg reg_frm : UInt<3>, clock with :
      reset => (UInt<1>("h0"), reg_frm) @[CSR.scala 267:20]
    reg _T_931 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), _T_931) @[Counters.scala 47:37]
    node _GEN_355 = pad(io_retire, 6) @[Counters.scala 48:33]
    node _T_932 = add(_T_931, _GEN_355) @[Counters.scala 48:33]
    reg _T_934 : UInt<58>, clock with :
      reset => (UInt<1>("h0"), _T_934) @[Counters.scala 52:27]
    node _T_935 = bits(_T_932, 6, 6) @[Counters.scala 53:20]
    node _T_937 = add(_T_934, UInt<58>("h1")) @[Counters.scala 53:43]
    node _T_938 = tail(_T_937, 1) @[Counters.scala 53:43]
    node _GEN_0 = mux(_T_935, _T_938, _T_934) @[Counters.scala 53:34]
    node _T_939 = cat(_T_934, _T_931) @[Cat.scala 30:58]
    reg _T_942 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), _T_942) @[Counters.scala 47:37]
    node _T_943 = add(_T_942, UInt<6>("h1")) @[Counters.scala 48:33]
    reg _T_945 : UInt<58>, clock with :
      reset => (UInt<1>("h0"), _T_945) @[Counters.scala 52:27]
    node _T_946 = bits(_T_943, 6, 6) @[Counters.scala 53:20]
    node _T_948 = add(_T_945, UInt<58>("h1")) @[Counters.scala 53:43]
    node _T_949 = tail(_T_948, 1) @[Counters.scala 53:43]
    node _GEN_1 = mux(_T_946, _T_949, _T_945) @[Counters.scala 53:34]
    node _T_950 = cat(_T_945, _T_942) @[Cat.scala 30:58]
    node _T_953 = eq(reg_mstatus_prv, UInt<2>("h1")) @[CSR.scala 274:71]
    skip
    node _T_956 = mux(_T_953, UInt<32>("h7"), reg_scounteren) @[CSR.scala 274:38]
    node hpm_mask = and(reg_mcounteren, _T_956) @[CSR.scala 274:33]
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
    node _T_974 = cat(cat(cat(reg_mip_stip, UInt<1>("h0")), reg_mip_msip), cat(cat(UInt<1>("h0"), reg_mip_ssip), UInt<1>("h0"))) @[CSR.scala 278:22]
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
    node _T_981 = cat(cat(cat(cat(io_rocc_interrupt, reg_mip_meip), cat(UInt<1>("h0"), reg_mip_seip)), cat(cat(UInt<1>("h0"), reg_mip_mtip), UInt<1>("h0"))), _T_974) @[CSR.scala 278:22]
    node read_mip = and(_T_981, UInt<13>("haaa")) @[CSR.scala 278:29]
    node _GEN_356 = pad(read_mip, 64) @[CSR.scala 280:37]
    node pending_interrupts = and(_GEN_356, reg_mie) @[CSR.scala 280:37]
    skip
    node _T_985 = eq(reg_mstatus_prv, UInt<2>("h3")) @[CSR.scala 281:71]
    node _T_986 = and(_T_985, reg_mstatus_mie) @[CSR.scala 281:81]
    node _T_987 = or(_T_2317, _T_986) @[CSR.scala 281:51]
    node _T_988 = not(reg_mideleg) @[CSR.scala 281:123]
    node _T_989 = and(pending_interrupts, _T_988) @[CSR.scala 281:121]
    node m_interrupts = mux(_T_987, _T_989, UInt<64>("h0")) @[CSR.scala 281:25]
    node _T_992 = eq(m_interrupts, UInt<64>("h0")) @[CSR.scala 282:39]
    node _T_994 = lt(reg_mstatus_prv, UInt<2>("h1")) @[CSR.scala 282:65]
    skip
    node _T_997 = and(_T_953, reg_mstatus_sie) @[CSR.scala 282:103]
    node _T_998 = or(_T_994, _T_997) @[CSR.scala 282:73]
    node _T_999 = and(_T_992, _T_998) @[CSR.scala 282:45]
    node _T_1000 = and(pending_interrupts, reg_mideleg) @[CSR.scala 282:144]
    node s_interrupts = mux(_T_999, _T_1000, UInt<64>("h0")) @[CSR.scala 282:25]
    node all_interrupts = or(m_interrupts, s_interrupts) @[CSR.scala 283:37]
    node _T_1003 = bits(all_interrupts, 0, 0) @[OneHot.scala 39:40]
    node _T_1004 = bits(all_interrupts, 1, 1) @[OneHot.scala 39:40]
    node _T_1005 = bits(all_interrupts, 2, 2) @[OneHot.scala 39:40]
    node _T_1006 = bits(all_interrupts, 3, 3) @[OneHot.scala 39:40]
    node _T_1007 = bits(all_interrupts, 4, 4) @[OneHot.scala 39:40]
    node _T_1008 = bits(all_interrupts, 5, 5) @[OneHot.scala 39:40]
    node _T_1009 = bits(all_interrupts, 6, 6) @[OneHot.scala 39:40]
    node _T_1010 = bits(all_interrupts, 7, 7) @[OneHot.scala 39:40]
    node _T_1011 = bits(all_interrupts, 8, 8) @[OneHot.scala 39:40]
    node _T_1012 = bits(all_interrupts, 9, 9) @[OneHot.scala 39:40]
    node _T_1013 = bits(all_interrupts, 10, 10) @[OneHot.scala 39:40]
    node _T_1014 = bits(all_interrupts, 11, 11) @[OneHot.scala 39:40]
    node _T_1015 = bits(all_interrupts, 12, 12) @[OneHot.scala 39:40]
    node _T_1016 = bits(all_interrupts, 13, 13) @[OneHot.scala 39:40]
    node _T_1017 = bits(all_interrupts, 14, 14) @[OneHot.scala 39:40]
    node _T_1018 = bits(all_interrupts, 15, 15) @[OneHot.scala 39:40]
    node _T_1019 = bits(all_interrupts, 16, 16) @[OneHot.scala 39:40]
    node _T_1020 = bits(all_interrupts, 17, 17) @[OneHot.scala 39:40]
    node _T_1021 = bits(all_interrupts, 18, 18) @[OneHot.scala 39:40]
    node _T_1022 = bits(all_interrupts, 19, 19) @[OneHot.scala 39:40]
    node _T_1023 = bits(all_interrupts, 20, 20) @[OneHot.scala 39:40]
    node _T_1024 = bits(all_interrupts, 21, 21) @[OneHot.scala 39:40]
    node _T_1025 = bits(all_interrupts, 22, 22) @[OneHot.scala 39:40]
    node _T_1026 = bits(all_interrupts, 23, 23) @[OneHot.scala 39:40]
    node _T_1027 = bits(all_interrupts, 24, 24) @[OneHot.scala 39:40]
    node _T_1028 = bits(all_interrupts, 25, 25) @[OneHot.scala 39:40]
    node _T_1029 = bits(all_interrupts, 26, 26) @[OneHot.scala 39:40]
    node _T_1030 = bits(all_interrupts, 27, 27) @[OneHot.scala 39:40]
    node _T_1031 = bits(all_interrupts, 28, 28) @[OneHot.scala 39:40]
    node _T_1032 = bits(all_interrupts, 29, 29) @[OneHot.scala 39:40]
    node _T_1033 = bits(all_interrupts, 30, 30) @[OneHot.scala 39:40]
    node _T_1034 = bits(all_interrupts, 31, 31) @[OneHot.scala 39:40]
    node _T_1035 = bits(all_interrupts, 32, 32) @[OneHot.scala 39:40]
    node _T_1036 = bits(all_interrupts, 33, 33) @[OneHot.scala 39:40]
    node _T_1037 = bits(all_interrupts, 34, 34) @[OneHot.scala 39:40]
    node _T_1038 = bits(all_interrupts, 35, 35) @[OneHot.scala 39:40]
    node _T_1039 = bits(all_interrupts, 36, 36) @[OneHot.scala 39:40]
    node _T_1040 = bits(all_interrupts, 37, 37) @[OneHot.scala 39:40]
    node _T_1041 = bits(all_interrupts, 38, 38) @[OneHot.scala 39:40]
    node _T_1042 = bits(all_interrupts, 39, 39) @[OneHot.scala 39:40]
    node _T_1043 = bits(all_interrupts, 40, 40) @[OneHot.scala 39:40]
    node _T_1044 = bits(all_interrupts, 41, 41) @[OneHot.scala 39:40]
    node _T_1045 = bits(all_interrupts, 42, 42) @[OneHot.scala 39:40]
    node _T_1046 = bits(all_interrupts, 43, 43) @[OneHot.scala 39:40]
    node _T_1047 = bits(all_interrupts, 44, 44) @[OneHot.scala 39:40]
    node _T_1048 = bits(all_interrupts, 45, 45) @[OneHot.scala 39:40]
    node _T_1049 = bits(all_interrupts, 46, 46) @[OneHot.scala 39:40]
    node _T_1050 = bits(all_interrupts, 47, 47) @[OneHot.scala 39:40]
    node _T_1051 = bits(all_interrupts, 48, 48) @[OneHot.scala 39:40]
    node _T_1052 = bits(all_interrupts, 49, 49) @[OneHot.scala 39:40]
    node _T_1053 = bits(all_interrupts, 50, 50) @[OneHot.scala 39:40]
    node _T_1054 = bits(all_interrupts, 51, 51) @[OneHot.scala 39:40]
    node _T_1055 = bits(all_interrupts, 52, 52) @[OneHot.scala 39:40]
    node _T_1056 = bits(all_interrupts, 53, 53) @[OneHot.scala 39:40]
    node _T_1057 = bits(all_interrupts, 54, 54) @[OneHot.scala 39:40]
    node _T_1058 = bits(all_interrupts, 55, 55) @[OneHot.scala 39:40]
    node _T_1059 = bits(all_interrupts, 56, 56) @[OneHot.scala 39:40]
    node _T_1060 = bits(all_interrupts, 57, 57) @[OneHot.scala 39:40]
    node _T_1061 = bits(all_interrupts, 58, 58) @[OneHot.scala 39:40]
    node _T_1062 = bits(all_interrupts, 59, 59) @[OneHot.scala 39:40]
    node _T_1063 = bits(all_interrupts, 60, 60) @[OneHot.scala 39:40]
    node _T_1064 = bits(all_interrupts, 61, 61) @[OneHot.scala 39:40]
    node _T_1065 = bits(all_interrupts, 62, 62) @[OneHot.scala 39:40]
    skip
    node _T_1131 = mux(_T_1065, UInt<6>("h3e"), UInt<6>("h3f")) @[Mux.scala 31:69]
    node _T_1132 = mux(_T_1064, UInt<6>("h3d"), _T_1131) @[Mux.scala 31:69]
    node _T_1133 = mux(_T_1063, UInt<6>("h3c"), _T_1132) @[Mux.scala 31:69]
    node _T_1134 = mux(_T_1062, UInt<6>("h3b"), _T_1133) @[Mux.scala 31:69]
    node _T_1135 = mux(_T_1061, UInt<6>("h3a"), _T_1134) @[Mux.scala 31:69]
    node _T_1136 = mux(_T_1060, UInt<6>("h39"), _T_1135) @[Mux.scala 31:69]
    node _T_1137 = mux(_T_1059, UInt<6>("h38"), _T_1136) @[Mux.scala 31:69]
    node _T_1138 = mux(_T_1058, UInt<6>("h37"), _T_1137) @[Mux.scala 31:69]
    node _T_1139 = mux(_T_1057, UInt<6>("h36"), _T_1138) @[Mux.scala 31:69]
    node _T_1140 = mux(_T_1056, UInt<6>("h35"), _T_1139) @[Mux.scala 31:69]
    node _T_1141 = mux(_T_1055, UInt<6>("h34"), _T_1140) @[Mux.scala 31:69]
    node _T_1142 = mux(_T_1054, UInt<6>("h33"), _T_1141) @[Mux.scala 31:69]
    node _T_1143 = mux(_T_1053, UInt<6>("h32"), _T_1142) @[Mux.scala 31:69]
    node _T_1144 = mux(_T_1052, UInt<6>("h31"), _T_1143) @[Mux.scala 31:69]
    node _T_1145 = mux(_T_1051, UInt<6>("h30"), _T_1144) @[Mux.scala 31:69]
    node _T_1146 = mux(_T_1050, UInt<6>("h2f"), _T_1145) @[Mux.scala 31:69]
    node _T_1147 = mux(_T_1049, UInt<6>("h2e"), _T_1146) @[Mux.scala 31:69]
    node _T_1148 = mux(_T_1048, UInt<6>("h2d"), _T_1147) @[Mux.scala 31:69]
    node _T_1149 = mux(_T_1047, UInt<6>("h2c"), _T_1148) @[Mux.scala 31:69]
    node _T_1150 = mux(_T_1046, UInt<6>("h2b"), _T_1149) @[Mux.scala 31:69]
    node _T_1151 = mux(_T_1045, UInt<6>("h2a"), _T_1150) @[Mux.scala 31:69]
    node _T_1152 = mux(_T_1044, UInt<6>("h29"), _T_1151) @[Mux.scala 31:69]
    node _T_1153 = mux(_T_1043, UInt<6>("h28"), _T_1152) @[Mux.scala 31:69]
    node _T_1154 = mux(_T_1042, UInt<6>("h27"), _T_1153) @[Mux.scala 31:69]
    node _T_1155 = mux(_T_1041, UInt<6>("h26"), _T_1154) @[Mux.scala 31:69]
    node _T_1156 = mux(_T_1040, UInt<6>("h25"), _T_1155) @[Mux.scala 31:69]
    node _T_1157 = mux(_T_1039, UInt<6>("h24"), _T_1156) @[Mux.scala 31:69]
    node _T_1158 = mux(_T_1038, UInt<6>("h23"), _T_1157) @[Mux.scala 31:69]
    node _T_1159 = mux(_T_1037, UInt<6>("h22"), _T_1158) @[Mux.scala 31:69]
    node _T_1160 = mux(_T_1036, UInt<6>("h21"), _T_1159) @[Mux.scala 31:69]
    node _T_1161 = mux(_T_1035, UInt<6>("h20"), _T_1160) @[Mux.scala 31:69]
    node _T_1162 = mux(_T_1034, UInt<6>("h1f"), _T_1161) @[Mux.scala 31:69]
    node _T_1163 = mux(_T_1033, UInt<6>("h1e"), _T_1162) @[Mux.scala 31:69]
    node _T_1164 = mux(_T_1032, UInt<6>("h1d"), _T_1163) @[Mux.scala 31:69]
    node _T_1165 = mux(_T_1031, UInt<6>("h1c"), _T_1164) @[Mux.scala 31:69]
    node _T_1166 = mux(_T_1030, UInt<6>("h1b"), _T_1165) @[Mux.scala 31:69]
    node _T_1167 = mux(_T_1029, UInt<6>("h1a"), _T_1166) @[Mux.scala 31:69]
    node _T_1168 = mux(_T_1028, UInt<6>("h19"), _T_1167) @[Mux.scala 31:69]
    node _T_1169 = mux(_T_1027, UInt<6>("h18"), _T_1168) @[Mux.scala 31:69]
    node _T_1170 = mux(_T_1026, UInt<6>("h17"), _T_1169) @[Mux.scala 31:69]
    node _T_1171 = mux(_T_1025, UInt<6>("h16"), _T_1170) @[Mux.scala 31:69]
    node _T_1172 = mux(_T_1024, UInt<6>("h15"), _T_1171) @[Mux.scala 31:69]
    node _T_1173 = mux(_T_1023, UInt<6>("h14"), _T_1172) @[Mux.scala 31:69]
    node _T_1174 = mux(_T_1022, UInt<6>("h13"), _T_1173) @[Mux.scala 31:69]
    node _T_1175 = mux(_T_1021, UInt<6>("h12"), _T_1174) @[Mux.scala 31:69]
    node _T_1176 = mux(_T_1020, UInt<6>("h11"), _T_1175) @[Mux.scala 31:69]
    node _T_1177 = mux(_T_1019, UInt<6>("h10"), _T_1176) @[Mux.scala 31:69]
    node _T_1178 = mux(_T_1018, UInt<6>("hf"), _T_1177) @[Mux.scala 31:69]
    node _T_1179 = mux(_T_1017, UInt<6>("he"), _T_1178) @[Mux.scala 31:69]
    node _T_1180 = mux(_T_1016, UInt<6>("hd"), _T_1179) @[Mux.scala 31:69]
    node _T_1181 = mux(_T_1015, UInt<6>("hc"), _T_1180) @[Mux.scala 31:69]
    node _T_1182 = mux(_T_1014, UInt<6>("hb"), _T_1181) @[Mux.scala 31:69]
    node _T_1183 = mux(_T_1013, UInt<6>("ha"), _T_1182) @[Mux.scala 31:69]
    node _T_1184 = mux(_T_1012, UInt<6>("h9"), _T_1183) @[Mux.scala 31:69]
    node _T_1185 = mux(_T_1011, UInt<6>("h8"), _T_1184) @[Mux.scala 31:69]
    node _T_1186 = mux(_T_1010, UInt<6>("h7"), _T_1185) @[Mux.scala 31:69]
    node _T_1187 = mux(_T_1009, UInt<6>("h6"), _T_1186) @[Mux.scala 31:69]
    node _T_1188 = mux(_T_1008, UInt<6>("h5"), _T_1187) @[Mux.scala 31:69]
    node _T_1189 = mux(_T_1007, UInt<6>("h4"), _T_1188) @[Mux.scala 31:69]
    node _T_1190 = mux(_T_1006, UInt<6>("h3"), _T_1189) @[Mux.scala 31:69]
    node _T_1191 = mux(_T_1005, UInt<6>("h2"), _T_1190) @[Mux.scala 31:69]
    node _T_1192 = mux(_T_1004, UInt<6>("h1"), _T_1191) @[Mux.scala 31:69]
    node _T_1193 = mux(_T_1003, UInt<6>("h0"), _T_1192) @[Mux.scala 31:69]
    node _GEN_357 = pad(_T_1193, 64) @[CSR.scala 285:43]
    node _T_1194 = add(UInt<64>("h8000000000000000"), _GEN_357) @[CSR.scala 285:43]
    node interruptCause = tail(_T_1194, 1) @[CSR.scala 285:43]
    node _T_1196 = neq(all_interrupts, UInt<64>("h0")) @[CSR.scala 286:34]
    node _T_1198 = eq(reg_debug, UInt<1>("h0")) @[CSR.scala 286:41]
    node _T_1199 = and(_T_1196, _T_1198) @[CSR.scala 286:38]
    node _T_1201 = eq(io_singleStep, UInt<1>("h0")) @[CSR.scala 286:55]
    node _T_1202 = and(_T_1199, _T_1201) @[CSR.scala 286:52]
    node _T_1203 = or(_T_1202, reg_singleStepped) @[CSR.scala 286:70]
    skip
    skip
    node _T_1208 = and(reg_dcsr_debugint, _T_1198) @[CSR.scala 291:47]
    skip
    skip
    skip
    skip
    reg reg_misa : UInt<64>, clock with :
      reset => (UInt<1>("h0"), reg_misa) @[CSR.scala 307:21]
    skip
    skip
    skip
    skip
    skip
    node _T_1234 = cat(cat(cat(io_status_hpie, io_status_spie), cat(io_status_upie, io_status_mie)), cat(cat(io_status_hie, io_status_sie), io_status_uie)) @[CSR.scala 308:38]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1241 = cat(cat(cat(cat(io_status_mprv, io_status_xs), cat(io_status_fs, io_status_mpp)), cat(cat(io_status_hpp, io_status_spp), io_status_mpie)), _T_1234) @[CSR.scala 308:38]
    skip
    skip
    skip
    skip
    skip
    node _T_1247 = cat(cat(cat(io_status_sd_rv32, io_status_zero1), cat(io_status_tsr, io_status_tw)), cat(cat(io_status_tvm, io_status_mxr), io_status_pum)) @[CSR.scala 308:38]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1255 = cat(cat(cat(cat(cat(io_status_debug, io_status_isa), cat(io_status_prv, io_status_sd)), cat(cat(io_status_zero2, io_status_sxl), io_status_uxl)), _T_1247), _T_1241) @[CSR.scala 308:38]
    node read_mstatus = bits(_T_1255, 63, 0) @[CSR.scala 308:40]
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
    node _T_1296 = cat(cat(cat(reg_bp_0_control_m, UInt<1>("h0")), cat(reg_bp_0_control_s, reg_bp_0_control_u)), cat(cat(reg_bp_0_control_x, reg_bp_0_control_w), reg_bp_0_control_r)) @[CSR.scala 312:48]
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
    node _T_1304 = cat(cat(cat(cat(UInt<4>("h2"), reg_bp_0_control_dmode), UInt<46>("h40000000000")), cat(cat(reg_bp_0_control_action, UInt<1>("h0")), cat(UInt<2>("h0"), reg_bp_0_control_tmatch))), _T_1296) @[CSR.scala 312:48]
    skip
    node _T_1340 = bits(reg_bp_0_address, 38, 38) @[Package.scala 40:38]
    skip
    node _T_1344 = mux(_T_1340, UInt<25>("h1ffffff"), UInt<25>("h0")) @[Bitwise.scala 71:12]
    skip
    node _T_1345 = cat(_T_1344, reg_bp_0_address) @[Cat.scala 30:58]
    node _T_1349 = bits(reg_mepc, 39, 39) @[Package.scala 40:38]
    skip
    node _T_1353 = mux(_T_1349, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 71:12]
    node _T_1354 = cat(_T_1353, reg_mepc) @[Cat.scala 30:58]
    node _T_1355 = bits(reg_mbadaddr, 39, 39) @[Package.scala 40:38]
    skip
    node _T_1359 = mux(_T_1355, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 71:12]
    node _T_1360 = cat(_T_1359, reg_mbadaddr) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1367 = cat(cat(UInt<2>("h0"), cat(reg_dcsr_cause, reg_dcsr_debugint)), cat(cat(UInt<1>("h0"), reg_dcsr_halt), cat(reg_dcsr_step, reg_dcsr_prv))) @[CSR.scala 333:27]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1376 = cat(cat(cat(UInt<4>("h4"), cat(UInt<12>("h0"), reg_dcsr_ebreakm)), cat(cat(UInt<1>("h0"), reg_dcsr_ebreaks), cat(reg_dcsr_ebreaku, UInt<1>("h0")))), _T_1367) @[CSR.scala 333:27]
    node _T_1377 = cat(reg_frm, reg_fflags) @[Cat.scala 30:58]
    node _T_1380 = and(reg_mie, reg_mideleg) @[CSR.scala 360:28]
    skip
    node _T_1381 = and(_GEN_356, reg_mideleg) @[CSR.scala 361:29]
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
    node _T_1423 = cat(cat(cat(UInt<1>("h0"), io_status_spie), cat(io_status_upie, UInt<1>("h0"))), cat(cat(UInt<1>("h0"), io_status_sie), io_status_uie)) @[CSR.scala 371:57]
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
    node _T_1430 = cat(cat(cat(cat(UInt<1>("h0"), io_status_xs), cat(io_status_fs, UInt<2>("h0"))), cat(cat(UInt<2>("h0"), io_status_spp), UInt<1>("h0"))), _T_1423) @[CSR.scala 371:57]
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
    node _T_1444 = cat(cat(cat(cat(cat(io_status_debug, io_status_isa), cat(io_status_prv, io_status_sd)), cat(cat(io_status_zero2, io_status_sxl), io_status_uxl)), _T_1247), _T_1430) @[CSR.scala 371:57]
    node _T_1445 = bits(_T_1444, 63, 0) @[CSR.scala 371:60]
    node _T_1446 = bits(reg_sbadaddr, 39, 39) @[Package.scala 40:38]
    skip
    node _T_1450 = mux(_T_1446, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 71:12]
    node _T_1451 = cat(_T_1450, reg_sbadaddr) @[Cat.scala 30:58]
    skip
    node _T_1453 = cat(cat(reg_sptbr_mode, UInt<16>("h0")), reg_sptbr_ppn) @[CSR.scala 377:45]
    node _T_1454 = bits(reg_sepc, 39, 39) @[Package.scala 40:38]
    skip
    node _T_1458 = mux(_T_1454, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 71:12]
    node _T_1459 = cat(_T_1458, reg_sepc) @[Cat.scala 30:58]
    node _T_1460 = bits(reg_stvec, 38, 38) @[Package.scala 40:38]
    skip
    node _T_1464 = mux(_T_1460, UInt<25>("h1ffffff"), UInt<25>("h0")) @[Bitwise.scala 71:12]
    node _T_1465 = cat(_T_1464, reg_stvec) @[Cat.scala 30:58]
    skip
    node _T_1469 = eq(io_rw_addr, UInt<12>("h7a1")) @[CSR.scala 405:73]
    node _T_1471 = eq(io_rw_addr, UInt<12>("h7a2")) @[CSR.scala 405:73]
    skip
    skip
    skip
    node _T_1479 = eq(io_rw_addr, UInt<12>("hb00")) @[CSR.scala 405:73]
    node _T_1481 = eq(io_rw_addr, UInt<12>("hb02")) @[CSR.scala 405:73]
    node _T_1483 = eq(io_rw_addr, UInt<12>("h301")) @[CSR.scala 405:73]
    node _T_1485 = eq(io_rw_addr, UInt<12>("h300")) @[CSR.scala 405:73]
    node _T_1487 = eq(io_rw_addr, UInt<12>("h305")) @[CSR.scala 405:73]
    node _T_1489 = eq(io_rw_addr, UInt<12>("h344")) @[CSR.scala 405:73]
    node _T_1491 = eq(io_rw_addr, UInt<12>("h304")) @[CSR.scala 405:73]
    node _T_1493 = eq(io_rw_addr, UInt<12>("h303")) @[CSR.scala 405:73]
    node _T_1495 = eq(io_rw_addr, UInt<12>("h302")) @[CSR.scala 405:73]
    node _T_1497 = eq(io_rw_addr, UInt<12>("h340")) @[CSR.scala 405:73]
    node _T_1499 = eq(io_rw_addr, UInt<12>("h341")) @[CSR.scala 405:73]
    node _T_1501 = eq(io_rw_addr, UInt<12>("h343")) @[CSR.scala 405:73]
    node _T_1503 = eq(io_rw_addr, UInt<12>("h342")) @[CSR.scala 405:73]
    node _T_1505 = eq(io_rw_addr, UInt<12>("hf14")) @[CSR.scala 405:73]
    node _T_1507 = eq(io_rw_addr, UInt<12>("h7b0")) @[CSR.scala 405:73]
    node _T_1509 = eq(io_rw_addr, UInt<12>("h7b1")) @[CSR.scala 405:73]
    node _T_1511 = eq(io_rw_addr, UInt<12>("h7b2")) @[CSR.scala 405:73]
    node _T_1513 = eq(io_rw_addr, UInt<12>("h1")) @[CSR.scala 405:73]
    node _T_1515 = eq(io_rw_addr, UInt<12>("h2")) @[CSR.scala 405:73]
    node _T_1517 = eq(io_rw_addr, UInt<12>("h3")) @[CSR.scala 405:73]
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
    skip
    skip
    skip
    node _T_1693 = eq(io_rw_addr, UInt<12>("h100")) @[CSR.scala 405:73]
    node _T_1695 = eq(io_rw_addr, UInt<12>("h144")) @[CSR.scala 405:73]
    node _T_1697 = eq(io_rw_addr, UInt<12>("h104")) @[CSR.scala 405:73]
    node _T_1699 = eq(io_rw_addr, UInt<12>("h140")) @[CSR.scala 405:73]
    node _T_1701 = eq(io_rw_addr, UInt<12>("h142")) @[CSR.scala 405:73]
    node _T_1703 = eq(io_rw_addr, UInt<12>("h143")) @[CSR.scala 405:73]
    node _T_1705 = eq(io_rw_addr, UInt<12>("h180")) @[CSR.scala 405:73]
    node _T_1707 = eq(io_rw_addr, UInt<12>("h141")) @[CSR.scala 405:73]
    node _T_1709 = eq(io_rw_addr, UInt<12>("h105")) @[CSR.scala 405:73]
    node _T_1711 = eq(io_rw_addr, UInt<12>("h106")) @[CSR.scala 405:73]
    node _T_1713 = eq(io_rw_addr, UInt<12>("h306")) @[CSR.scala 405:73]
    node _T_1715 = eq(io_rw_addr, UInt<12>("hc00")) @[CSR.scala 405:73]
    node _T_1717 = eq(io_rw_addr, UInt<12>("hc02")) @[CSR.scala 405:73]
    node _T_1720 = eq(io_rw_cmd, UInt<3>("h2")) @[Package.scala 7:47]
    node _T_1721 = eq(io_rw_cmd, UInt<3>("h3")) @[Package.scala 7:47]
    node _T_1722 = or(_T_1720, _T_1721) @[Package.scala 7:62]
    node _T_1724 = mux(_T_1722, io_rw_rdata, UInt<64>("h0")) @[CSR.scala 406:19]
    node _T_1725 = or(_T_1724, io_rw_wdata) @[CSR.scala 406:75]
    skip
    node _T_1729 = mux(_T_1721, io_rw_wdata, UInt<64>("h0")) @[CSR.scala 407:19]
    node _T_1730 = not(_T_1729) @[CSR.scala 407:15]
    node wdata = and(_T_1725, _T_1730) @[CSR.scala 406:90]
    node _T_1740 = bits(opcode, 5, 5) @[CSR.scala 415:39]
    node insn_wfi = and(system_insn, _T_1740) @[CSR.scala 415:30]
    skip
    node _T_1743 = gt(effective_prv, UInt<3>("h1")) @[CSR.scala 418:51]
    skip
    node _T_1746 = eq(reg_mstatus_tw, UInt<1>("h0")) @[CSR.scala 418:62]
    node allow_wfi = or(_T_1743, _T_1746) @[CSR.scala 418:59]
    skip
    skip
    node _T_1752 = eq(reg_mstatus_tvm, UInt<1>("h0")) @[CSR.scala 419:69]
    node allow_sfence_vma = or(_T_1743, _T_1752) @[CSR.scala 419:66]
    skip
    skip
    node _T_1758 = eq(reg_mstatus_tsr, UInt<1>("h0")) @[CSR.scala 420:63]
    node allow_sret = or(_T_1743, _T_1758) @[CSR.scala 420:60]
    node _T_1760 = eq(io_status_fs, UInt<2>("h0")) @[CSR.scala 421:40]
    node _T_1761 = bits(reg_misa, 5, 5) @[CSR.scala 421:58]
    node _T_1763 = eq(_T_1761, UInt<1>("h0")) @[CSR.scala 421:49]
    skip
    skip
    skip
    skip
    skip
    node _T_1771 = bits(io_decode_csr, 9, 8) @[CSR.scala 423:58]
    node _GEN_359 = pad(_T_1771, 3) @[CSR.scala 423:43]
    node _T_1772 = lt(effective_prv, _GEN_359) @[CSR.scala 423:43]
    node _T_1774 = eq(io_decode_csr, UInt<12>("h7a0")) @[CSR.scala 424:42]
    node _T_1776 = eq(io_decode_csr, UInt<12>("h7a1")) @[CSR.scala 424:42]
    node _T_1778 = eq(io_decode_csr, UInt<12>("h7a2")) @[CSR.scala 424:42]
    node _T_1780 = eq(io_decode_csr, UInt<12>("hf13")) @[CSR.scala 424:42]
    node _T_1782 = eq(io_decode_csr, UInt<12>("hf12")) @[CSR.scala 424:42]
    node _T_1784 = eq(io_decode_csr, UInt<12>("hf11")) @[CSR.scala 424:42]
    node _T_1786 = eq(io_decode_csr, UInt<12>("hb00")) @[CSR.scala 424:42]
    node _T_1788 = eq(io_decode_csr, UInt<12>("hb02")) @[CSR.scala 424:42]
    node _T_1790 = eq(io_decode_csr, UInt<12>("h301")) @[CSR.scala 424:42]
    node _T_1792 = eq(io_decode_csr, UInt<12>("h300")) @[CSR.scala 424:42]
    node _T_1794 = eq(io_decode_csr, UInt<12>("h305")) @[CSR.scala 424:42]
    node _T_1796 = eq(io_decode_csr, UInt<12>("h344")) @[CSR.scala 424:42]
    node _T_1798 = eq(io_decode_csr, UInt<12>("h304")) @[CSR.scala 424:42]
    node _T_1800 = eq(io_decode_csr, UInt<12>("h303")) @[CSR.scala 424:42]
    node _T_1802 = eq(io_decode_csr, UInt<12>("h302")) @[CSR.scala 424:42]
    node _T_1804 = eq(io_decode_csr, UInt<12>("h340")) @[CSR.scala 424:42]
    node _T_1806 = eq(io_decode_csr, UInt<12>("h341")) @[CSR.scala 424:42]
    node _T_1808 = eq(io_decode_csr, UInt<12>("h343")) @[CSR.scala 424:42]
    node _T_1810 = eq(io_decode_csr, UInt<12>("h342")) @[CSR.scala 424:42]
    node _T_1812 = eq(io_decode_csr, UInt<12>("hf14")) @[CSR.scala 424:42]
    node _T_1814 = eq(io_decode_csr, UInt<12>("h7b0")) @[CSR.scala 424:42]
    node _T_1816 = eq(io_decode_csr, UInt<12>("h7b1")) @[CSR.scala 424:42]
    node _T_1818 = eq(io_decode_csr, UInt<12>("h7b2")) @[CSR.scala 424:42]
    node _T_1820 = eq(io_decode_csr, UInt<12>("h1")) @[CSR.scala 424:42]
    node _T_1822 = eq(io_decode_csr, UInt<12>("h2")) @[CSR.scala 424:42]
    node _T_1824 = eq(io_decode_csr, UInt<12>("h3")) @[CSR.scala 424:42]
    node _T_1826 = eq(io_decode_csr, UInt<12>("h323")) @[CSR.scala 424:42]
    node _T_1828 = eq(io_decode_csr, UInt<12>("hb03")) @[CSR.scala 424:42]
    node _T_1830 = eq(io_decode_csr, UInt<12>("hc03")) @[CSR.scala 424:42]
    node _T_1832 = eq(io_decode_csr, UInt<12>("h324")) @[CSR.scala 424:42]
    node _T_1834 = eq(io_decode_csr, UInt<12>("hb04")) @[CSR.scala 424:42]
    node _T_1836 = eq(io_decode_csr, UInt<12>("hc04")) @[CSR.scala 424:42]
    node _T_1838 = eq(io_decode_csr, UInt<12>("h325")) @[CSR.scala 424:42]
    node _T_1840 = eq(io_decode_csr, UInt<12>("hb05")) @[CSR.scala 424:42]
    node _T_1842 = eq(io_decode_csr, UInt<12>("hc05")) @[CSR.scala 424:42]
    node _T_1844 = eq(io_decode_csr, UInt<12>("h326")) @[CSR.scala 424:42]
    node _T_1846 = eq(io_decode_csr, UInt<12>("hb06")) @[CSR.scala 424:42]
    node _T_1848 = eq(io_decode_csr, UInt<12>("hc06")) @[CSR.scala 424:42]
    node _T_1850 = eq(io_decode_csr, UInt<12>("h327")) @[CSR.scala 424:42]
    node _T_1852 = eq(io_decode_csr, UInt<12>("hb07")) @[CSR.scala 424:42]
    node _T_1854 = eq(io_decode_csr, UInt<12>("hc07")) @[CSR.scala 424:42]
    node _T_1856 = eq(io_decode_csr, UInt<12>("h328")) @[CSR.scala 424:42]
    node _T_1858 = eq(io_decode_csr, UInt<12>("hb08")) @[CSR.scala 424:42]
    node _T_1860 = eq(io_decode_csr, UInt<12>("hc08")) @[CSR.scala 424:42]
    node _T_1862 = eq(io_decode_csr, UInt<12>("h329")) @[CSR.scala 424:42]
    node _T_1864 = eq(io_decode_csr, UInt<12>("hb09")) @[CSR.scala 424:42]
    node _T_1866 = eq(io_decode_csr, UInt<12>("hc09")) @[CSR.scala 424:42]
    node _T_1868 = eq(io_decode_csr, UInt<12>("h32a")) @[CSR.scala 424:42]
    node _T_1870 = eq(io_decode_csr, UInt<12>("hb0a")) @[CSR.scala 424:42]
    node _T_1872 = eq(io_decode_csr, UInt<12>("hc0a")) @[CSR.scala 424:42]
    node _T_1874 = eq(io_decode_csr, UInt<12>("h32b")) @[CSR.scala 424:42]
    node _T_1876 = eq(io_decode_csr, UInt<12>("hb0b")) @[CSR.scala 424:42]
    node _T_1878 = eq(io_decode_csr, UInt<12>("hc0b")) @[CSR.scala 424:42]
    node _T_1880 = eq(io_decode_csr, UInt<12>("h32c")) @[CSR.scala 424:42]
    node _T_1882 = eq(io_decode_csr, UInt<12>("hb0c")) @[CSR.scala 424:42]
    node _T_1884 = eq(io_decode_csr, UInt<12>("hc0c")) @[CSR.scala 424:42]
    node _T_1886 = eq(io_decode_csr, UInt<12>("h32d")) @[CSR.scala 424:42]
    node _T_1888 = eq(io_decode_csr, UInt<12>("hb0d")) @[CSR.scala 424:42]
    node _T_1890 = eq(io_decode_csr, UInt<12>("hc0d")) @[CSR.scala 424:42]
    node _T_1892 = eq(io_decode_csr, UInt<12>("h32e")) @[CSR.scala 424:42]
    node _T_1894 = eq(io_decode_csr, UInt<12>("hb0e")) @[CSR.scala 424:42]
    node _T_1896 = eq(io_decode_csr, UInt<12>("hc0e")) @[CSR.scala 424:42]
    node _T_1898 = eq(io_decode_csr, UInt<12>("h32f")) @[CSR.scala 424:42]
    node _T_1900 = eq(io_decode_csr, UInt<12>("hb0f")) @[CSR.scala 424:42]
    node _T_1902 = eq(io_decode_csr, UInt<12>("hc0f")) @[CSR.scala 424:42]
    node _T_1904 = eq(io_decode_csr, UInt<12>("h330")) @[CSR.scala 424:42]
    node _T_1906 = eq(io_decode_csr, UInt<12>("hb10")) @[CSR.scala 424:42]
    node _T_1908 = eq(io_decode_csr, UInt<12>("hc10")) @[CSR.scala 424:42]
    node _T_1910 = eq(io_decode_csr, UInt<12>("h331")) @[CSR.scala 424:42]
    node _T_1912 = eq(io_decode_csr, UInt<12>("hb11")) @[CSR.scala 424:42]
    node _T_1914 = eq(io_decode_csr, UInt<12>("hc11")) @[CSR.scala 424:42]
    node _T_1916 = eq(io_decode_csr, UInt<12>("h332")) @[CSR.scala 424:42]
    node _T_1918 = eq(io_decode_csr, UInt<12>("hb12")) @[CSR.scala 424:42]
    node _T_1920 = eq(io_decode_csr, UInt<12>("hc12")) @[CSR.scala 424:42]
    node _T_1922 = eq(io_decode_csr, UInt<12>("h333")) @[CSR.scala 424:42]
    node _T_1924 = eq(io_decode_csr, UInt<12>("hb13")) @[CSR.scala 424:42]
    node _T_1926 = eq(io_decode_csr, UInt<12>("hc13")) @[CSR.scala 424:42]
    node _T_1928 = eq(io_decode_csr, UInt<12>("h334")) @[CSR.scala 424:42]
    node _T_1930 = eq(io_decode_csr, UInt<12>("hb14")) @[CSR.scala 424:42]
    node _T_1932 = eq(io_decode_csr, UInt<12>("hc14")) @[CSR.scala 424:42]
    node _T_1934 = eq(io_decode_csr, UInt<12>("h335")) @[CSR.scala 424:42]
    node _T_1936 = eq(io_decode_csr, UInt<12>("hb15")) @[CSR.scala 424:42]
    node _T_1938 = eq(io_decode_csr, UInt<12>("hc15")) @[CSR.scala 424:42]
    node _T_1940 = eq(io_decode_csr, UInt<12>("h336")) @[CSR.scala 424:42]
    node _T_1942 = eq(io_decode_csr, UInt<12>("hb16")) @[CSR.scala 424:42]
    node _T_1944 = eq(io_decode_csr, UInt<12>("hc16")) @[CSR.scala 424:42]
    node _T_1946 = eq(io_decode_csr, UInt<12>("h337")) @[CSR.scala 424:42]
    node _T_1948 = eq(io_decode_csr, UInt<12>("hb17")) @[CSR.scala 424:42]
    node _T_1950 = eq(io_decode_csr, UInt<12>("hc17")) @[CSR.scala 424:42]
    node _T_1952 = eq(io_decode_csr, UInt<12>("h338")) @[CSR.scala 424:42]
    node _T_1954 = eq(io_decode_csr, UInt<12>("hb18")) @[CSR.scala 424:42]
    node _T_1956 = eq(io_decode_csr, UInt<12>("hc18")) @[CSR.scala 424:42]
    node _T_1958 = eq(io_decode_csr, UInt<12>("h339")) @[CSR.scala 424:42]
    node _T_1960 = eq(io_decode_csr, UInt<12>("hb19")) @[CSR.scala 424:42]
    node _T_1962 = eq(io_decode_csr, UInt<12>("hc19")) @[CSR.scala 424:42]
    node _T_1964 = eq(io_decode_csr, UInt<12>("h33a")) @[CSR.scala 424:42]
    node _T_1966 = eq(io_decode_csr, UInt<12>("hb1a")) @[CSR.scala 424:42]
    node _T_1968 = eq(io_decode_csr, UInt<12>("hc1a")) @[CSR.scala 424:42]
    node _T_1970 = eq(io_decode_csr, UInt<12>("h33b")) @[CSR.scala 424:42]
    node _T_1972 = eq(io_decode_csr, UInt<12>("hb1b")) @[CSR.scala 424:42]
    node _T_1974 = eq(io_decode_csr, UInt<12>("hc1b")) @[CSR.scala 424:42]
    node _T_1976 = eq(io_decode_csr, UInt<12>("h33c")) @[CSR.scala 424:42]
    node _T_1978 = eq(io_decode_csr, UInt<12>("hb1c")) @[CSR.scala 424:42]
    node _T_1980 = eq(io_decode_csr, UInt<12>("hc1c")) @[CSR.scala 424:42]
    node _T_1982 = eq(io_decode_csr, UInt<12>("h33d")) @[CSR.scala 424:42]
    node _T_1984 = eq(io_decode_csr, UInt<12>("hb1d")) @[CSR.scala 424:42]
    node _T_1986 = eq(io_decode_csr, UInt<12>("hc1d")) @[CSR.scala 424:42]
    node _T_1988 = eq(io_decode_csr, UInt<12>("h33e")) @[CSR.scala 424:42]
    node _T_1990 = eq(io_decode_csr, UInt<12>("hb1e")) @[CSR.scala 424:42]
    node _T_1992 = eq(io_decode_csr, UInt<12>("hc1e")) @[CSR.scala 424:42]
    node _T_1994 = eq(io_decode_csr, UInt<12>("h33f")) @[CSR.scala 424:42]
    node _T_1996 = eq(io_decode_csr, UInt<12>("hb1f")) @[CSR.scala 424:42]
    node _T_1998 = eq(io_decode_csr, UInt<12>("hc1f")) @[CSR.scala 424:42]
    node _T_2000 = eq(io_decode_csr, UInt<12>("h100")) @[CSR.scala 424:42]
    node _T_2002 = eq(io_decode_csr, UInt<12>("h144")) @[CSR.scala 424:42]
    node _T_2004 = eq(io_decode_csr, UInt<12>("h104")) @[CSR.scala 424:42]
    node _T_2006 = eq(io_decode_csr, UInt<12>("h140")) @[CSR.scala 424:42]
    node _T_2008 = eq(io_decode_csr, UInt<12>("h142")) @[CSR.scala 424:42]
    node _T_2010 = eq(io_decode_csr, UInt<12>("h143")) @[CSR.scala 424:42]
    node _T_2012 = eq(io_decode_csr, UInt<12>("h180")) @[CSR.scala 424:42]
    node _T_2014 = eq(io_decode_csr, UInt<12>("h141")) @[CSR.scala 424:42]
    node _T_2016 = eq(io_decode_csr, UInt<12>("h105")) @[CSR.scala 424:42]
    node _T_2018 = eq(io_decode_csr, UInt<12>("h106")) @[CSR.scala 424:42]
    node _T_2020 = eq(io_decode_csr, UInt<12>("h306")) @[CSR.scala 424:42]
    node _T_2022 = eq(io_decode_csr, UInt<12>("hc00")) @[CSR.scala 424:42]
    node _T_2024 = eq(io_decode_csr, UInt<12>("hc02")) @[CSR.scala 424:42]
    node _T_2025 = or(_T_1952, _T_1820) @[CSR.scala 424:57]
    node _T_2026 = or(_T_2025, _T_1864) @[CSR.scala 424:57]
    node _T_2027 = or(_T_2026, _T_2006) @[CSR.scala 424:57]
    node _T_2028 = or(_T_2027, _T_1978) @[CSR.scala 424:57]
    node _T_2029 = or(_T_2028, _T_1946) @[CSR.scala 424:57]
    node _T_2030 = or(_T_2029, _T_1832) @[CSR.scala 424:57]
    node _T_2031 = or(_T_2030, _T_1800) @[CSR.scala 424:57]
    node _T_2032 = or(_T_2031, _T_2020) @[CSR.scala 424:57]
    node _T_2033 = or(_T_2032, _T_1914) @[CSR.scala 424:57]
    node _T_2034 = or(_T_2033, _T_1876) @[CSR.scala 424:57]
    node _T_2035 = or(_T_2034, _T_1920) @[CSR.scala 424:57]
    node _T_2036 = or(_T_2035, _T_1788) @[CSR.scala 424:57]
    node _T_2037 = or(_T_2036, _T_1844) @[CSR.scala 424:57]
    node _T_2038 = or(_T_2037, _T_1888) @[CSR.scala 424:57]
    node _T_2039 = or(_T_2038, _T_1872) @[CSR.scala 424:57]
    node _T_2040 = or(_T_2039, _T_1988) @[CSR.scala 424:57]
    node _T_2041 = or(_T_2040, _T_1910) @[CSR.scala 424:57]
    node _T_2042 = or(_T_2041, _T_1924) @[CSR.scala 424:57]
    node _T_2043 = or(_T_2042, _T_1776) @[CSR.scala 424:57]
    node _T_2044 = or(_T_2043, _T_1808) @[CSR.scala 424:57]
    node _T_2045 = or(_T_2044, _T_1892) @[CSR.scala 424:57]
    node _T_2046 = or(_T_2045, _T_1840) @[CSR.scala 424:57]
    node _T_2047 = or(_T_2046, _T_1956) @[CSR.scala 424:57]
    node _T_2048 = or(_T_2047, _T_2010) @[CSR.scala 424:57]
    node _T_2049 = or(_T_2048, _T_1868) @[CSR.scala 424:57]
    node _T_2050 = or(_T_2049, _T_2000) @[CSR.scala 424:57]
    node _T_2051 = or(_T_2050, _T_1878) @[CSR.scala 424:57]
    node _T_2052 = or(_T_2051, _T_1942) @[CSR.scala 424:57]
    node _T_2053 = or(_T_2052, _T_1812) @[CSR.scala 424:57]
    node _T_2054 = or(_T_2053, _T_1974) @[CSR.scala 424:57]
    node _T_2055 = or(_T_2054, _T_1968) @[CSR.scala 424:57]
    node _T_2056 = or(_T_2055, _T_1780) @[CSR.scala 424:57]
    node _T_2057 = or(_T_2056, _T_1836) @[CSR.scala 424:57]
    node _T_2058 = or(_T_2057, _T_1996) @[CSR.scala 424:57]
    node _T_2059 = or(_T_2058, _T_1814) @[CSR.scala 424:57]
    node _T_2060 = or(_T_2059, _T_1900) @[CSR.scala 424:57]
    node _T_2061 = or(_T_2060, _T_1846) @[CSR.scala 424:57]
    node _T_2062 = or(_T_2061, _T_1932) @[CSR.scala 424:57]
    node _T_2063 = or(_T_2062, _T_1964) @[CSR.scala 424:57]
    node _T_2064 = or(_T_2063, _T_1782) @[CSR.scala 424:57]
    node _T_2065 = or(_T_2064, _T_1904) @[CSR.scala 424:57]
    node _T_2066 = or(_T_2065, _T_1992) @[CSR.scala 424:57]
    node _T_2067 = or(_T_2066, _T_1860) @[CSR.scala 424:57]
    node _T_2068 = or(_T_2067, _T_1804) @[CSR.scala 424:57]
    node _T_2069 = or(_T_2068, _T_1936) @[CSR.scala 424:57]
    node _T_2070 = or(_T_2069, _T_1960) @[CSR.scala 424:57]
    node _T_2071 = or(_T_2070, _T_1828) @[CSR.scala 424:57]
    node _T_2072 = or(_T_2071, _T_1908) @[CSR.scala 424:57]
    node _T_2073 = or(_T_2072, _T_2024) @[CSR.scala 424:57]
    node _T_2074 = or(_T_2073, _T_2004) @[CSR.scala 424:57]
    node _T_2075 = or(_T_2074, _T_1856) @[CSR.scala 424:57]
    node _T_2076 = or(_T_2075, _T_1940) @[CSR.scala 424:57]
    node _T_2077 = or(_T_2076, _T_1792) @[CSR.scala 424:57]
    node _T_2078 = or(_T_2077, _T_1972) @[CSR.scala 424:57]
    node _T_2079 = or(_T_2078, _T_1824) @[CSR.scala 424:57]
    node _T_2080 = or(_T_2079, _T_1896) @[CSR.scala 424:57]
    node _T_2081 = or(_T_2080, _T_1928) @[CSR.scala 424:57]
    node _T_2082 = or(_T_2081, _T_1796) @[CSR.scala 424:57]
    node _T_2083 = or(_T_2082, _T_1918) @[CSR.scala 424:57]
    node _T_2084 = or(_T_2083, _T_1786) @[CSR.scala 424:57]
    node _T_2085 = or(_T_2084, _T_1980) @[CSR.scala 424:57]
    node _T_2086 = or(_T_2085, _T_1774) @[CSR.scala 424:57]
    node _T_2087 = or(_T_2086, _T_1830) @[CSR.scala 424:57]
    node _T_2088 = or(_T_2087, _T_1890) @[CSR.scala 424:57]
    node _T_2089 = or(_T_2088, _T_2018) @[CSR.scala 424:57]
    node _T_2090 = or(_T_2089, _T_1916) @[CSR.scala 424:57]
    node _T_2091 = or(_T_2090, _T_1884) @[CSR.scala 424:57]
    node _T_2092 = or(_T_2091, _T_1862) @[CSR.scala 424:57]
    node _T_2093 = or(_T_2092, _T_1948) @[CSR.scala 424:57]
    node _T_2094 = or(_T_2093, _T_1798) @[CSR.scala 424:57]
    node _T_2095 = or(_T_2094, _T_1976) @[CSR.scala 424:57]
    node _T_2096 = or(_T_2095, _T_1886) @[CSR.scala 424:57]
    node _T_2097 = or(_T_2096, _T_1950) @[CSR.scala 424:57]
    node _T_2098 = or(_T_2097, _T_1818) @[CSR.scala 424:57]
    node _T_2099 = or(_T_2098, _T_1982) @[CSR.scala 424:57]
    node _T_2100 = or(_T_2099, _T_1850) @[CSR.scala 424:57]
    node _T_2101 = or(_T_2100, _T_1810) @[CSR.scala 424:57]
    node _T_2102 = or(_T_2101, _T_1944) @[CSR.scala 424:57]
    node _T_2103 = or(_T_2102, _T_2014) @[CSR.scala 424:57]
    node _T_2104 = or(_T_2103, _T_1838) @[CSR.scala 424:57]
    node _T_2105 = or(_T_2104, _T_1954) @[CSR.scala 424:57]
    node _T_2106 = or(_T_2105, _T_1870) @[CSR.scala 424:57]
    node _T_2107 = or(_T_2106, _T_1986) @[CSR.scala 424:57]
    node _T_2108 = or(_T_2107, _T_2008) @[CSR.scala 424:57]
    node _T_2109 = or(_T_2108, _T_1922) @[CSR.scala 424:57]
    node _T_2110 = or(_T_2109, _T_1778) @[CSR.scala 424:57]
    node _T_2111 = or(_T_2110, _T_1806) @[CSR.scala 424:57]
    node _T_2112 = or(_T_2111, _T_1842) @[CSR.scala 424:57]
    node _T_2113 = or(_T_2112, _T_1874) @[CSR.scala 424:57]
    node _T_2114 = or(_T_2113, _T_1880) @[CSR.scala 424:57]
    node _T_2115 = or(_T_2114, _T_2012) @[CSR.scala 424:57]
    node _T_2116 = or(_T_2115, _T_1912) @[CSR.scala 424:57]
    node _T_2117 = or(_T_2116, _T_1802) @[CSR.scala 424:57]
    node _T_2118 = or(_T_2117, _T_1934) @[CSR.scala 424:57]
    node _T_2119 = or(_T_2118, _T_1848) @[CSR.scala 424:57]
    node _T_2120 = or(_T_2119, _T_1962) @[CSR.scala 424:57]
    node _T_2121 = or(_T_2120, _T_1784) @[CSR.scala 424:57]
    node _T_2122 = or(_T_2121, _T_1902) @[CSR.scala 424:57]
    node _T_2123 = or(_T_2122, _T_1994) @[CSR.scala 424:57]
    node _T_2124 = or(_T_2123, _T_2016) @[CSR.scala 424:57]
    node _T_2125 = or(_T_2124, _T_1930) @[CSR.scala 424:57]
    node _T_2126 = or(_T_2125, _T_1816) @[CSR.scala 424:57]
    node _T_2127 = or(_T_2126, _T_1834) @[CSR.scala 424:57]
    node _T_2128 = or(_T_2127, _T_1966) @[CSR.scala 424:57]
    node _T_2129 = or(_T_2128, _T_1898) @[CSR.scala 424:57]
    node _T_2130 = or(_T_2129, _T_1866) @[CSR.scala 424:57]
    node _T_2131 = or(_T_2130, _T_1998) @[CSR.scala 424:57]
    node _T_2132 = or(_T_2131, _T_1926) @[CSR.scala 424:57]
    node _T_2133 = or(_T_2132, _T_1794) @[CSR.scala 424:57]
    node _T_2134 = or(_T_2133, _T_1882) @[CSR.scala 424:57]
    node _T_2135 = or(_T_2134, _T_1970) @[CSR.scala 424:57]
    node _T_2136 = or(_T_2135, _T_1822) @[CSR.scala 424:57]
    node _T_2137 = or(_T_2136, _T_1894) @[CSR.scala 424:57]
    node _T_2138 = or(_T_2137, _T_2002) @[CSR.scala 424:57]
    node _T_2139 = or(_T_2138, _T_1854) @[CSR.scala 424:57]
    node _T_2140 = or(_T_2139, _T_1938) @[CSR.scala 424:57]
    node _T_2141 = or(_T_2140, _T_1984) @[CSR.scala 424:57]
    node _T_2142 = or(_T_2141, _T_1790) @[CSR.scala 424:57]
    node _T_2143 = or(_T_2142, _T_1852) @[CSR.scala 424:57]
    node _T_2144 = or(_T_2143, _T_1958) @[CSR.scala 424:57]
    node _T_2145 = or(_T_2144, _T_1826) @[CSR.scala 424:57]
    node _T_2146 = or(_T_2145, _T_2022) @[CSR.scala 424:57]
    node _T_2147 = or(_T_2146, _T_1906) @[CSR.scala 424:57]
    node _T_2148 = or(_T_2147, _T_1990) @[CSR.scala 424:57]
    node _T_2149 = or(_T_2148, _T_1858) @[CSR.scala 424:57]
    node _T_2151 = eq(_T_2149, UInt<1>("h0")) @[CSR.scala 424:5]
    node _T_2152 = or(_T_1772, _T_2151) @[CSR.scala 423:64]
    skip
    node _T_2156 = eq(allow_sfence_vma, UInt<1>("h0")) @[CSR.scala 425:37]
    node _T_2157 = and(_T_2012, _T_2156) @[CSR.scala 425:34]
    node _T_2158 = or(_T_2152, _T_2157) @[CSR.scala 424:62]
    node _T_2161 = geq(io_decode_csr, UInt<12>("hc00")) @[Package.scala 47:47]
    node _T_2162 = lt(io_decode_csr, UInt<12>("hc20")) @[Package.scala 47:60]
    node _T_2163 = and(_T_2161, _T_2162) @[Package.scala 47:55]
    node _T_2166 = geq(io_decode_csr, UInt<12>("hc80")) @[Package.scala 47:47]
    node _T_2167 = lt(io_decode_csr, UInt<12>("hca0")) @[Package.scala 47:60]
    node _T_2168 = and(_T_2166, _T_2167) @[Package.scala 47:55]
    node _T_2169 = or(_T_2163, _T_2168) @[CSR.scala 426:67]
    node _T_2171 = leq(effective_prv, UInt<3>("h1")) @[CSR.scala 426:151]
    node _T_2172 = and(_T_2169, _T_2171) @[CSR.scala 426:134]
    skip
    node _T_2174 = dshr(hpm_mask, io_decode_csr) @[CSR.scala 426:171]
    node _T_2175 = bits(_T_2174, 0, 0) @[CSR.scala 426:171]
    node _T_2176 = and(_T_2172, _T_2175) @[CSR.scala 426:160]
    node _T_2177 = or(_T_2158, _T_2176) @[CSR.scala 425:55]
    skip
    skip
    skip
    skip
    skip
    node _T_2188 = or(_T_1814, _T_1816) @[CSR.scala 427:88]
    node _T_2189 = or(_T_2188, _T_1818) @[CSR.scala 427:88]
    node _T_2190 = and(_T_1198, _T_2189) @[CSR.scala 427:36]
    node _T_2191 = or(_T_2177, _T_2190) @[CSR.scala 426:215]
    skip
    skip
    skip
    node _T_2199 = or(_T_1820, _T_1822) @[CSR.scala 428:69]
    node _T_2200 = or(_T_2199, _T_1824) @[CSR.scala 428:69]
    skip
    node _T_2202 = and(_T_2200, io_decode_fp_illegal) @[CSR.scala 428:74]
    skip
    node _T_2204 = bits(io_decode_csr, 11, 10) @[CSR.scala 429:43]
    node _T_2205 = not(_T_2204) @[CSR.scala 429:51]
    skip
    node _T_2209 = geq(io_decode_csr, UInt<12>("h340")) @[CSR.scala 430:44]
    node _T_2211 = leq(io_decode_csr, UInt<12>("h343")) @[CSR.scala 430:78]
    node _T_2212 = and(_T_2209, _T_2211) @[CSR.scala 430:61]
    node _T_2214 = geq(io_decode_csr, UInt<12>("h140")) @[CSR.scala 430:112]
    node _T_2216 = leq(io_decode_csr, UInt<12>("h143")) @[CSR.scala 430:146]
    node _T_2217 = and(_T_2214, _T_2216) @[CSR.scala 430:129]
    node _T_2218 = or(_T_2212, _T_2217) @[CSR.scala 430:95]
    skip
    skip
    skip
    skip
    node _T_2223 = bits(io_decode_csr, 5, 5) @[CSR.scala 432:19]
    node _T_2225 = eq(_T_2223, UInt<1>("h0")) @[CSR.scala 432:5]
    node _T_2226 = bits(io_decode_csr, 2, 2) @[CSR.scala 432:39]
    node _T_2227 = and(_T_2225, _T_2226) @[CSR.scala 432:23]
    node _T_2229 = eq(allow_wfi, UInt<1>("h0")) @[CSR.scala 432:46]
    node _T_2230 = and(_T_2227, _T_2229) @[CSR.scala 432:43]
    node _T_2231 = or(_T_1772, _T_2230) @[CSR.scala 431:66]
    skip
    skip
    node _T_2235 = bits(io_decode_csr, 1, 1) @[CSR.scala 433:39]
    node _T_2236 = and(_T_2225, _T_2235) @[CSR.scala 433:23]
    node _T_2238 = eq(allow_sret, UInt<1>("h0")) @[CSR.scala 433:46]
    node _T_2239 = and(_T_2236, _T_2238) @[CSR.scala 433:43]
    node _T_2240 = or(_T_2231, _T_2239) @[CSR.scala 432:57]
    skip
    skip
    node _T_2244 = and(_T_2223, _T_2156) @[CSR.scala 434:22]
    skip
    node debugTVec = mux(reg_debug, UInt<12>("h808"), UInt<12>("h800")) @[CSR.scala 445:22]
    skip
    node _T_2328 = cat(_T_1460, reg_stvec) @[Cat.scala 30:58]
    node _T_2329 = mux(delegate, _T_2328, pad(reg_mtvec, 40)) @[CSR.scala 446:45]
    node tvec = mux(trapToDebug, pad(debugTVec, 40), _T_2329) @[CSR.scala 446:17]
    skip
    skip
    skip
    skip
    node _T_2335 = not(io_status_fs) @[CSR.scala 453:32]
    node _T_2337 = eq(_T_2335, UInt<2>("h0")) @[CSR.scala 453:32]
    node _T_2338 = not(io_status_xs) @[CSR.scala 453:53]
    node _T_2340 = eq(_T_2338, UInt<2>("h0")) @[CSR.scala 453:53]
    skip
    node _T_2345 = add(insn_ret, insn_call) @[Bitwise.scala 48:55]
    node _T_2346 = add(insn_break, io_exception) @[Bitwise.scala 48:55]
    node _T_2347 = add(_T_2345, _T_2346) @[Bitwise.scala 48:55]
    node _T_2349 = leq(_T_2347, UInt<3>("h1")) @[CSR.scala 462:79]
    node _T_2350 = or(_T_2349, reset) @[CSR.scala 462:9]
    node _T_2352 = eq(_T_2350, UInt<1>("h0")) @[CSR.scala 462:9]
    node _GEN_36 = or(insn_wfi, reg_wfi) @[CSR.scala 464:19]
    node _T_2355 = neq(pending_interrupts, UInt<64>("h0")) @[CSR.scala 465:28]
    node _T_2356 = or(_T_2355, exception) @[CSR.scala 465:32]
    node _GEN_37 = mux(_T_2356, UInt<1>("h0"), _GEN_36) @[CSR.scala 465:46]
    node _T_2359 = eq(reg_wfi, UInt<1>("h0")) @[CSR.scala 466:10]
    node _T_2361 = eq(io_retire, UInt<1>("h0")) @[CSR.scala 466:32]
    node _T_2362 = or(_T_2359, _T_2361) @[CSR.scala 466:19]
    node _T_2363 = or(_T_2362, reset) @[CSR.scala 466:9]
    node _T_2365 = eq(_T_2363, UInt<1>("h0")) @[CSR.scala 466:9]
    skip
    node _GEN_38 = or(io_retire, reg_singleStepped) @[CSR.scala 468:23]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_2380 = eq(reg_singleStepped, UInt<1>("h0")) @[CSR.scala 471:10]
    skip
    node _T_2383 = or(_T_2380, _T_2361) @[CSR.scala 471:29]
    node _T_2384 = or(_T_2383, reset) @[CSR.scala 471:9]
    node _T_2386 = eq(_T_2384, UInt<1>("h0")) @[CSR.scala 471:9]
    node _T_2387 = not(io_pc) @[CSR.scala 474:17]
    node _T_2389 = or(_T_2387, UInt<40>("h1")) @[CSR.scala 474:24]
    node _T_2390 = not(_T_2389) @[CSR.scala 474:15]
    node _T_2391 = dshr(read_mstatus, reg_mstatus_prv) @[CSR.scala 475:27]
    node _T_2392 = bits(_T_2391, 0, 0) @[CSR.scala 475:27]
    node _T_2400 = eq(cause, UInt<64>("h3")) @[Package.scala 7:47]
    node _T_2401 = eq(cause, UInt<64>("h4")) @[Package.scala 7:47]
    node _T_2402 = eq(cause, UInt<64>("h6")) @[Package.scala 7:47]
    node _T_2403 = eq(cause, UInt<64>("h0")) @[Package.scala 7:47]
    node _T_2404 = eq(cause, UInt<64>("h5")) @[Package.scala 7:47]
    node _T_2405 = eq(cause, UInt<64>("h7")) @[Package.scala 7:47]
    node _T_2406 = eq(cause, UInt<64>("h1")) @[Package.scala 7:47]
    node _T_2407 = or(_T_2400, _T_2401) @[Package.scala 7:62]
    node _T_2408 = or(_T_2407, _T_2402) @[Package.scala 7:62]
    node _T_2409 = or(_T_2408, _T_2403) @[Package.scala 7:62]
    node _T_2410 = or(_T_2409, _T_2404) @[Package.scala 7:62]
    node _T_2411 = or(_T_2410, _T_2405) @[Package.scala 7:62]
    node _T_2412 = or(_T_2411, _T_2406) @[Package.scala 7:62]
    node _T_2418 = mux(causeIsDebugTrigger, UInt<2>("h2"), UInt<2>("h1")) @[CSR.scala 484:84]
    node _T_2419 = mux(causeIsDebugInt, UInt<2>("h3"), _T_2418) @[CSR.scala 484:54]
    node _T_2420 = mux(reg_singleStepped, UInt<3>("h4"), pad(_T_2419, 3)) @[CSR.scala 484:28]
    node _GEN_40 = or(trapToDebug, reg_debug) @[CSR.scala 481:24]
    node _GEN_41 = mux(trapToDebug, _T_2390, reg_dpc) @[CSR.scala 481:24]
    node _GEN_42 = mux(trapToDebug, _T_2420, reg_dcsr_cause) @[CSR.scala 481:24]
    node _GEN_43 = mux(trapToDebug, reg_mstatus_prv, reg_dcsr_prv) @[CSR.scala 481:24]
    node _T_2424 = not(_T_2390) @[CSR.scala 714:28]
    node _T_2425 = bits(reg_misa, 2, 2) @[CSR.scala 714:46]
    node _T_2427 = eq(_T_2425, UInt<1>("h0")) @[CSR.scala 714:37]
    node _T_2429 = cat(_T_2427, UInt<1>("h1")) @[Cat.scala 30:58]
    node _GEN_361 = pad(_T_2429, 40) @[CSR.scala 714:31]
    node _T_2430 = or(_T_2424, _GEN_361) @[CSR.scala 714:31]
    node _T_2431 = not(_T_2430) @[CSR.scala 714:26]
    node _GEN_44 = mux(_T_2412, io_badaddr, reg_sbadaddr) @[CSR.scala 489:28]
    node _GEN_45 = mux(_T_2423, _T_2431, reg_sepc) @[CSR.scala 486:27]
    node _GEN_46 = mux(_T_2423, cause, reg_scause) @[CSR.scala 486:27]
    node _GEN_47 = mux(_T_2423, _GEN_44, reg_sbadaddr) @[CSR.scala 486:27]
    node _GEN_48 = mux(_T_2423, _T_2392, reg_mstatus_spie) @[CSR.scala 486:27]
    node _GEN_49 = mux(_T_2423, reg_mstatus_prv, pad(reg_mstatus_spp, 2)) @[CSR.scala 486:27]
    node _GEN_50 = mux(_T_2423, UInt<1>("h0"), reg_mstatus_sie) @[CSR.scala 486:27]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _GEN_52 = mux(_T_2412, io_badaddr, reg_mbadaddr) @[CSR.scala 497:28]
    node _GEN_53 = mux(_T_2438, _T_2431, reg_mepc) @[CSR.scala 494:17]
    node _GEN_54 = mux(_T_2438, cause, reg_mcause) @[CSR.scala 494:17]
    node _GEN_55 = mux(_T_2438, _GEN_52, reg_mbadaddr) @[CSR.scala 494:17]
    node _GEN_56 = mux(_T_2438, _T_2392, reg_mstatus_mpie) @[CSR.scala 494:17]
    node _GEN_57 = mux(_T_2438, reg_mstatus_prv, reg_mstatus_mpp) @[CSR.scala 494:17]
    node _GEN_58 = mux(_T_2438, UInt<1>("h0"), reg_mstatus_mie) @[CSR.scala 494:17]
    node _GEN_60 = mux(exception, _GEN_40, reg_debug) @[CSR.scala 473:20]
    node _GEN_61 = mux(exception, _GEN_41, reg_dpc) @[CSR.scala 473:20]
    node _GEN_62 = mux(exception, _GEN_42, reg_dcsr_cause) @[CSR.scala 473:20]
    node _GEN_63 = mux(exception, _GEN_43, reg_dcsr_prv) @[CSR.scala 473:20]
    node _GEN_64 = mux(exception, _GEN_45, reg_sepc) @[CSR.scala 473:20]
    node _GEN_65 = mux(exception, _GEN_46, reg_scause) @[CSR.scala 473:20]
    node _GEN_66 = mux(exception, _GEN_47, reg_sbadaddr) @[CSR.scala 473:20]
    node _GEN_67 = mux(exception, _GEN_48, reg_mstatus_spie) @[CSR.scala 473:20]
    node _GEN_68 = mux(exception, _GEN_49, pad(reg_mstatus_spp, 2)) @[CSR.scala 473:20]
    node _GEN_69 = mux(exception, _GEN_50, reg_mstatus_sie) @[CSR.scala 473:20]
    node _GEN_71 = mux(exception, _GEN_53, reg_mepc) @[CSR.scala 473:20]
    node _GEN_72 = mux(exception, _GEN_54, reg_mcause) @[CSR.scala 473:20]
    node _GEN_73 = mux(exception, _GEN_55, reg_mbadaddr) @[CSR.scala 473:20]
    node _GEN_74 = mux(exception, _GEN_56, reg_mstatus_mpie) @[CSR.scala 473:20]
    node _GEN_75 = mux(exception, _GEN_57, reg_mstatus_mpp) @[CSR.scala 473:20]
    node _GEN_76 = mux(exception, _GEN_58, reg_mstatus_mie) @[CSR.scala 473:20]
    skip
    node _GEN_77 = mux(reg_mstatus_spp, reg_mstatus_spie, _GEN_69) @[CSR.scala 507:37]
    node _GEN_78 = mux(_T_2452, _GEN_77, _GEN_69) @[CSR.scala 506:44]
    node _GEN_79 = or(_T_2452, _GEN_67) @[CSR.scala 506:44]
    node _GEN_80 = mux(_T_2452, UInt<2>("h0"), _GEN_68) @[CSR.scala 506:44]
    node _GEN_82 = mux(_T_2452, reg_sepc, tvec) @[CSR.scala 506:44]
    node _GEN_84 = mux(_T_2462, UInt<1>("h0"), _GEN_60) @[CSR.scala 512:53]
    node _GEN_85 = mux(_T_2462, reg_dpc, _GEN_82) @[CSR.scala 512:53]
    node _T_2469 = bits(reg_mstatus_mpp, 1, 1) @[CSR.scala 517:28]
    node _GEN_86 = mux(_T_2469, reg_mstatus_mpie, _GEN_76) @[CSR.scala 517:33]
    node _T_2471 = bits(reg_mstatus_mpp, 0, 0) @[CSR.scala 518:50]
    skip
    node _T_2474 = eq(_T_2469, UInt<1>("h0")) @[CSR.scala 517:33]
    node _T_2475 = and(_T_2474, _T_2471) @[CSR.scala 518:55]
    node _GEN_87 = mux(_T_2475, reg_mstatus_mpie, _GEN_78) @[CSR.scala 518:55]
    skip
    skip
    node _GEN_88 = mux(_T_2468, _GEN_86, _GEN_76) @[CSR.scala 516:17]
    node _GEN_89 = mux(_T_2468, _GEN_87, _GEN_78) @[CSR.scala 516:17]
    node _GEN_90 = or(_T_2468, _GEN_74) @[CSR.scala 516:17]
    node _GEN_91 = mux(_T_2468, UInt<2>("h0"), _GEN_75) @[CSR.scala 516:17]
    node _GEN_93 = mux(_T_2468, reg_mepc, _GEN_85) @[CSR.scala 516:17]
    node _GEN_94 = mux(insn_ret, _GEN_89, _GEN_69) @[CSR.scala 505:19]
    node _GEN_95 = mux(insn_ret, _GEN_79, _GEN_67) @[CSR.scala 505:19]
    node _GEN_96 = mux(insn_ret, _GEN_80, _GEN_68) @[CSR.scala 505:19]
    skip
    node _GEN_99 = mux(insn_ret, _GEN_84, _GEN_60) @[CSR.scala 505:19]
    node _GEN_100 = mux(insn_ret, _GEN_88, _GEN_76) @[CSR.scala 505:19]
    node _GEN_101 = mux(insn_ret, _GEN_90, _GEN_74) @[CSR.scala 505:19]
    node _GEN_102 = mux(insn_ret, _GEN_91, _GEN_75) @[CSR.scala 505:19]
    skip
    node _T_2485 = mux(_T_1469, _T_1304, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2487 = mux(_T_1471, _T_1345, UInt<64>("h0")) @[Mux.scala 19:72]
    skip
    skip
    skip
    node _T_2495 = mux(_T_1479, _T_950, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2497 = mux(_T_1481, _T_939, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2499 = mux(_T_1483, reg_misa, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2501 = mux(_T_1485, read_mstatus, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2503 = mux(_T_1487, reg_mtvec, UInt<32>("h0")) @[Mux.scala 19:72]
    node _T_2505 = mux(_T_1489, read_mip, UInt<13>("h0")) @[Mux.scala 19:72]
    node _T_2507 = mux(_T_1491, reg_mie, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2509 = mux(_T_1493, reg_mideleg, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2511 = mux(_T_1495, reg_medeleg, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2513 = mux(_T_1497, reg_mscratch, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2515 = mux(_T_1499, _T_1354, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2517 = mux(_T_1501, _T_1360, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2519 = mux(_T_1503, reg_mcause, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2521 = mux(_T_1505, io_hartid, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2523 = mux(_T_1507, _T_1376, UInt<32>("h0")) @[Mux.scala 19:72]
    node _T_2525 = mux(_T_1509, reg_dpc, UInt<40>("h0")) @[Mux.scala 19:72]
    node _T_2527 = mux(_T_1511, reg_dscratch, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2529 = mux(_T_1513, reg_fflags, UInt<5>("h0")) @[Mux.scala 19:72]
    node _T_2531 = mux(_T_1515, reg_frm, UInt<3>("h0")) @[Mux.scala 19:72]
    node _T_2533 = mux(_T_1517, _T_1377, UInt<8>("h0")) @[Mux.scala 19:72]
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
    skip
    skip
    skip
    node _T_2709 = mux(_T_1693, _T_1445, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2711 = mux(_T_1695, _T_1381, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2713 = mux(_T_1697, _T_1380, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2715 = mux(_T_1699, reg_sscratch, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2717 = mux(_T_1701, reg_scause, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2719 = mux(_T_1703, _T_1451, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2721 = mux(_T_1705, _T_1453, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2723 = mux(_T_1707, _T_1459, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2725 = mux(_T_1709, _T_1465, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2727 = mux(_T_1711, reg_scounteren, UInt<32>("h0")) @[Mux.scala 19:72]
    node _T_2729 = mux(_T_1713, reg_mcounteren, UInt<32>("h0")) @[Mux.scala 19:72]
    node _T_2731 = mux(_T_1715, _T_950, UInt<64>("h0")) @[Mux.scala 19:72]
    node _T_2733 = mux(_T_1717, _T_939, UInt<64>("h0")) @[Mux.scala 19:72]
    skip
    node _T_2736 = or(_T_2485, _T_2487) @[Mux.scala 19:72]
    skip
    skip
    skip
    node _T_2740 = or(_T_2736, _T_2495) @[Mux.scala 19:72]
    node _T_2741 = or(_T_2740, _T_2497) @[Mux.scala 19:72]
    node _T_2742 = or(_T_2741, _T_2499) @[Mux.scala 19:72]
    node _T_2743 = or(_T_2742, _T_2501) @[Mux.scala 19:72]
    node _GEN_363 = pad(_T_2503, 64) @[Mux.scala 19:72]
    node _T_2744 = or(_T_2743, _GEN_363) @[Mux.scala 19:72]
    node _GEN_364 = pad(_T_2505, 64) @[Mux.scala 19:72]
    node _T_2745 = or(_T_2744, _GEN_364) @[Mux.scala 19:72]
    node _T_2746 = or(_T_2745, _T_2507) @[Mux.scala 19:72]
    node _T_2747 = or(_T_2746, _T_2509) @[Mux.scala 19:72]
    node _T_2748 = or(_T_2747, _T_2511) @[Mux.scala 19:72]
    node _T_2749 = or(_T_2748, _T_2513) @[Mux.scala 19:72]
    node _T_2750 = or(_T_2749, _T_2515) @[Mux.scala 19:72]
    node _T_2751 = or(_T_2750, _T_2517) @[Mux.scala 19:72]
    node _T_2752 = or(_T_2751, _T_2519) @[Mux.scala 19:72]
    node _T_2753 = or(_T_2752, _T_2521) @[Mux.scala 19:72]
    node _GEN_365 = pad(_T_2523, 64) @[Mux.scala 19:72]
    node _T_2754 = or(_T_2753, _GEN_365) @[Mux.scala 19:72]
    node _GEN_366 = pad(_T_2525, 64) @[Mux.scala 19:72]
    node _T_2755 = or(_T_2754, _GEN_366) @[Mux.scala 19:72]
    node _T_2756 = or(_T_2755, _T_2527) @[Mux.scala 19:72]
    node _GEN_367 = pad(_T_2529, 64) @[Mux.scala 19:72]
    node _T_2757 = or(_T_2756, _GEN_367) @[Mux.scala 19:72]
    node _GEN_368 = pad(_T_2531, 64) @[Mux.scala 19:72]
    node _T_2758 = or(_T_2757, _GEN_368) @[Mux.scala 19:72]
    node _GEN_369 = pad(_T_2533, 64) @[Mux.scala 19:72]
    node _T_2759 = or(_T_2758, _GEN_369) @[Mux.scala 19:72]
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
    skip
    skip
    skip
    node _T_2847 = or(_T_2759, _T_2709) @[Mux.scala 19:72]
    node _T_2848 = or(_T_2847, _T_2711) @[Mux.scala 19:72]
    node _T_2849 = or(_T_2848, _T_2713) @[Mux.scala 19:72]
    node _T_2850 = or(_T_2849, _T_2715) @[Mux.scala 19:72]
    node _T_2851 = or(_T_2850, _T_2717) @[Mux.scala 19:72]
    node _T_2852 = or(_T_2851, _T_2719) @[Mux.scala 19:72]
    node _T_2853 = or(_T_2852, _T_2721) @[Mux.scala 19:72]
    node _T_2854 = or(_T_2853, _T_2723) @[Mux.scala 19:72]
    node _T_2855 = or(_T_2854, _T_2725) @[Mux.scala 19:72]
    node _GEN_370 = pad(_T_2727, 64) @[Mux.scala 19:72]
    node _T_2856 = or(_T_2855, _GEN_370) @[Mux.scala 19:72]
    node _GEN_371 = pad(_T_2729, 64) @[Mux.scala 19:72]
    node _T_2857 = or(_T_2856, _GEN_371) @[Mux.scala 19:72]
    node _T_2858 = or(_T_2857, _T_2731) @[Mux.scala 19:72]
    skip
    node _T_2862 = or(reg_fflags, io_fcsr_flags_bits) @[CSR.scala 533:30]
    node _GEN_103 = mux(io_fcsr_flags_valid, _T_2862, reg_fflags) @[CSR.scala 532:30]
    skip
    skip
    node _T_2868 = eq(io_rw_cmd, UInt<3>("h1")) @[Package.scala 7:47]
    skip
    node _T_2870 = or(_T_1722, _T_2868) @[Package.scala 7:62]
    node _T_2959 = pad(wdata, 99)
    skip
    node _T_2961 = bits(_T_2959, 1, 1) @[CSR.scala 538:47]
    skip
    node _T_2963 = bits(_T_2959, 3, 3) @[CSR.scala 538:47]
    skip
    node _T_2965 = bits(_T_2959, 5, 5) @[CSR.scala 538:47]
    skip
    node _T_2967 = bits(_T_2959, 7, 7) @[CSR.scala 538:47]
    node _T_2968 = bits(_T_2959, 8, 8) @[CSR.scala 538:47]
    skip
    node _T_2970 = bits(_T_2959, 12, 11) @[CSR.scala 538:47]
    node _T_2971 = bits(_T_2959, 14, 13) @[CSR.scala 538:47]
    skip
    node _T_2973 = bits(_T_2959, 17, 17) @[CSR.scala 538:47]
    node _T_2974 = bits(_T_2959, 18, 18) @[CSR.scala 538:47]
    node _T_2975 = bits(_T_2959, 19, 19) @[CSR.scala 538:47]
    node _T_2976 = bits(_T_2959, 20, 20) @[CSR.scala 538:47]
    node _T_2977 = bits(_T_2959, 21, 21) @[CSR.scala 538:47]
    node _T_2978 = bits(_T_2959, 22, 22) @[CSR.scala 538:47]
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
    node _T_2989 = neq(_T_2971, UInt<2>("h0")) @[CSR.scala 557:73]
    skip
    node _T_2993 = mux(_T_2989, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 71:12]
    skip
    node _GEN_104 = mux(_T_1485, _T_2963, _GEN_100) @[CSR.scala 537:39]
    skip
    node _GEN_105 = mux(_T_1485, _T_2967, _GEN_101) @[CSR.scala 537:39]
    skip
    node _GEN_106 = mux(_T_1485, _T_2973, reg_mstatus_mprv) @[CSR.scala 537:39]
    skip
    node _GEN_107 = mux(_T_1485, _T_2970, _GEN_102) @[CSR.scala 537:39]
    skip
    node _GEN_108 = mux(_T_1485, _T_2975, reg_mstatus_mxr) @[CSR.scala 537:39]
    skip
    node _GEN_109 = mux(_T_1485, _T_2974, reg_mstatus_pum) @[CSR.scala 537:39]
    skip
    node _GEN_110 = mux(_T_1485, pad(_T_2968, 2), _GEN_96) @[CSR.scala 537:39]
    skip
    node _GEN_111 = mux(_T_1485, _T_2965, _GEN_95) @[CSR.scala 537:39]
    skip
    node _GEN_112 = mux(_T_1485, _T_2961, _GEN_94) @[CSR.scala 537:39]
    skip
    node _GEN_113 = mux(_T_1485, _T_2977, reg_mstatus_tw) @[CSR.scala 537:39]
    skip
    node _GEN_114 = mux(_T_1485, _T_2976, reg_mstatus_tvm) @[CSR.scala 537:39]
    skip
    node _GEN_115 = mux(_T_1485, _T_2978, reg_mstatus_tsr) @[CSR.scala 537:39]
    node _GEN_116 = mux(_T_1485, _T_2993, reg_mstatus_fs) @[CSR.scala 537:39]
    node _T_2995 = bits(wdata, 5, 5) @[CSR.scala 562:20]
    node _T_2996 = not(wdata) @[CSR.scala 563:21]
    node _T_2998 = eq(_T_2995, UInt<1>("h0")) @[CSR.scala 563:31]
    node _T_2999 = shl(_T_2998, 3) @[CSR.scala 563:34]
    node _GEN_372 = pad(_T_2999, 64) @[CSR.scala 563:28]
    node _T_3000 = or(_T_2996, _GEN_372) @[CSR.scala 563:28]
    node _T_3001 = not(_T_3000) @[CSR.scala 563:19]
    node _T_3002 = and(_T_3001, UInt<64>("h102d")) @[CSR.scala 563:51]
    skip
    node _T_3004 = and(reg_misa, UInt<64>("hfd2")) @[CSR.scala 563:69]
    node _T_3005 = or(_T_3002, _T_3004) @[CSR.scala 563:58]
    node _GEN_117 = mux(_T_1483, _T_3005, reg_misa) @[CSR.scala 560:36]
    node _T_3049 = bits(wdata, 12, 0)
    skip
    node _T_3051 = bits(_T_3049, 1, 1) @[CSR.scala 566:39]
    skip
    skip
    skip
    node _T_3055 = bits(_T_3049, 5, 5) @[CSR.scala 566:39]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _GEN_118 = mux(_T_1489, _T_3051, reg_mip_ssip) @[CSR.scala 565:35]
    skip
    node _GEN_119 = mux(_T_1489, _T_3055, reg_mip_stip) @[CSR.scala 565:35]
    node _T_3063 = and(wdata, UInt<64>("haaa")) @[CSR.scala 572:59]
    node _GEN_120 = mux(_T_1491, _T_3063, reg_mie) @[CSR.scala 572:40]
    skip
    skip
    skip
    skip
    node _GEN_373 = pad(_T_2429, 64) @[CSR.scala 714:31]
    node _T_3070 = or(_T_2996, _GEN_373) @[CSR.scala 714:31]
    node _T_3071 = not(_T_3070) @[CSR.scala 714:26]
    node _GEN_121 = mux(_T_1499, _T_3071, pad(_GEN_71, 64)) @[CSR.scala 573:40]
    node _GEN_122 = mux(_T_1497, wdata, reg_mscratch) @[CSR.scala 574:40]
    node _T_3072 = shr(wdata, 2) @[CSR.scala 576:61]
    node _T_3073 = shl(_T_3072, 2) @[CSR.scala 576:66]
    node _GEN_123 = mux(_T_1487, _T_3073, pad(reg_mtvec, 64)) @[CSR.scala 576:40]
    node _T_3075 = and(wdata, UInt<64>("h800000000000001f")) @[CSR.scala 577:62]
    node _GEN_124 = mux(_T_1503, _T_3075, _GEN_72) @[CSR.scala 577:40]
    node _T_3076 = bits(wdata, 39, 0) @[CSR.scala 578:63]
    node _GEN_125 = mux(_T_1501, _T_3076, _GEN_73) @[CSR.scala 578:40]
    skip
    node _T_3078 = shr(wdata, 6) @[Counters.scala 68:28]
    node _GEN_126 = mux(_T_1479, wdata, pad(_T_943, 64)) @[CSR.scala 711:31]
    node _GEN_127 = mux(_T_1479, _T_3078, _GEN_1) @[CSR.scala 711:31]
    skip
    skip
    node _GEN_128 = mux(_T_1481, wdata, pad(_T_932, 64)) @[CSR.scala 711:31]
    node _GEN_129 = mux(_T_1481, _T_3078, _GEN_0) @[CSR.scala 711:31]
    node _GEN_130 = mux(_T_1513, wdata, pad(_GEN_103, 64)) @[CSR.scala 588:40]
    node _GEN_131 = mux(_T_1515, wdata, pad(reg_frm, 64)) @[CSR.scala 589:40]
    node _T_3081 = shr(wdata, 5) @[CSR.scala 590:80]
    node _GEN_132 = mux(_T_1517, wdata, _GEN_130) @[CSR.scala 590:40]
    node _GEN_133 = mux(_T_1517, pad(_T_3081, 64), _GEN_131) @[CSR.scala 590:40]
    node _T_3137 = bits(wdata, 31, 0)
    node _T_3138 = bits(_T_3137, 1, 0) @[CSR.scala 594:43]
    node _T_3139 = bits(_T_3137, 2, 2) @[CSR.scala 594:43]
    node _T_3140 = bits(_T_3137, 3, 3) @[CSR.scala 594:43]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_3147 = bits(_T_3137, 12, 12) @[CSR.scala 594:43]
    node _T_3148 = bits(_T_3137, 13, 13) @[CSR.scala 594:43]
    skip
    node _T_3150 = bits(_T_3137, 15, 15) @[CSR.scala 594:43]
    skip
    skip
    skip
    skip
    skip
    node _GEN_134 = mux(_T_1507, _T_3140, reg_dcsr_halt) @[CSR.scala 593:38]
    skip
    node _GEN_135 = mux(_T_1507, _T_3139, reg_dcsr_step) @[CSR.scala 593:38]
    skip
    node _GEN_136 = mux(_T_1507, _T_3150, reg_dcsr_ebreakm) @[CSR.scala 593:38]
    skip
    node _GEN_137 = mux(_T_1507, _T_3148, reg_dcsr_ebreaks) @[CSR.scala 593:38]
    skip
    node _GEN_138 = mux(_T_1507, _T_3147, reg_dcsr_ebreaku) @[CSR.scala 593:38]
    skip
    node _GEN_139 = mux(_T_1507, _T_3138, _GEN_63) @[CSR.scala 593:38]
    skip
    node _T_3157 = or(_T_2996, UInt<64>("h1")) @[CSR.scala 602:64]
    node _T_3158 = not(_T_3157) @[CSR.scala 602:55]
    node _GEN_140 = mux(_T_1509, _T_3158, pad(_GEN_61, 64)) @[CSR.scala 602:42]
    node _GEN_141 = mux(_T_1511, wdata, reg_dscratch) @[CSR.scala 603:42]
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
    node _GEN_142 = mux(_T_1693, _T_2961, _GEN_112) @[CSR.scala 606:41]
    skip
    node _GEN_143 = mux(_T_1693, _T_2965, _GEN_111) @[CSR.scala 606:41]
    skip
    node _GEN_144 = mux(_T_1693, pad(_T_2968, 2), _GEN_110) @[CSR.scala 606:41]
    skip
    node _GEN_145 = mux(_T_1693, _T_2974, _GEN_109) @[CSR.scala 606:41]
    node _GEN_146 = mux(_T_1693, _T_2993, _GEN_116) @[CSR.scala 606:41]
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
    node _GEN_147 = mux(_T_1695, _T_3051, _GEN_118) @[CSR.scala 615:37]
    skip
    node _T_3353 = bits(wdata, 43, 0) @[CSR.scala 620:44]
    skip
    node _T_3355 = bits(wdata, 63, 60) @[CSR.scala 620:44]
    skip
    node _T_3357 = eq(_T_3355, UInt<4>("h0")) @[CSR.scala 622:30]
    node _GEN_148 = mux(_T_3357, UInt<4>("h0"), reg_sptbr_mode) @[CSR.scala 622:37]
    node _T_3360 = eq(_T_3355, UInt<4>("h8")) @[CSR.scala 623:30]
    node _GEN_149 = mux(_T_3360, UInt<4>("h8"), _GEN_148) @[CSR.scala 623:46]
    skip
    skip
    node _T_3366 = or(_T_3357, _T_3360) @[CSR.scala 624:36]
    skip
    node _T_3367 = bits(_T_3353, 19, 0) @[CSR.scala 625:41]
    node _GEN_150 = mux(_T_3366, pad(_T_3367, 44), reg_sptbr_ppn) @[CSR.scala 624:70]
    node _GEN_151 = mux(_T_1705, _GEN_149, reg_sptbr_mode) @[CSR.scala 619:39]
    node _GEN_152 = mux(_T_1705, _GEN_150, reg_sptbr_ppn) @[CSR.scala 619:39]
    skip
    node _T_3369 = and(reg_mie, _T_988) @[CSR.scala 629:64]
    node _T_3370 = and(wdata, reg_mideleg) @[CSR.scala 629:89]
    node _T_3371 = or(_T_3369, _T_3370) @[CSR.scala 629:80]
    node _GEN_153 = mux(_T_1697, _T_3371, _GEN_120) @[CSR.scala 629:42]
    node _GEN_154 = mux(_T_1699, wdata, reg_sscratch) @[CSR.scala 630:42]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _GEN_155 = mux(_T_1707, _T_3071, pad(_GEN_64, 64)) @[CSR.scala 631:42]
    skip
    skip
    node _GEN_156 = mux(_T_1709, _T_3073, pad(reg_stvec, 64)) @[CSR.scala 632:42]
    skip
    node _GEN_157 = mux(_T_1701, _T_3075, _GEN_65) @[CSR.scala 633:42]
    skip
    node _GEN_158 = mux(_T_1703, _T_3076, _GEN_66) @[CSR.scala 634:42]
    node _T_3385 = and(wdata, UInt<64>("h222")) @[CSR.scala 635:65]
    node _GEN_159 = mux(_T_1493, _T_3385, reg_mideleg) @[CSR.scala 635:42]
    node _T_3386 = and(wdata, UInt<64>("h1ab")) @[CSR.scala 636:65]
    node _GEN_160 = mux(_T_1495, _T_3386, reg_medeleg) @[CSR.scala 636:42]
    node _T_3388 = and(wdata, UInt<64>("h7")) @[CSR.scala 637:70]
    node _GEN_161 = mux(_T_1711, _T_3388, pad(reg_scounteren, 64)) @[CSR.scala 637:44]
    skip
    node _GEN_162 = mux(_T_1713, _T_3388, pad(reg_mcounteren, 64)) @[CSR.scala 640:44]
    skip
    skip
    node _T_3427 = eq(reg_bp_0_control_dmode, UInt<1>("h0")) @[CSR.scala 646:13]
    node _T_3428 = or(_T_3427, reg_debug) @[CSR.scala 646:31]
    skip
    node _T_3479 = bits(wdata, 0, 0) @[CSR.scala 648:48]
    node _T_3480 = bits(wdata, 1, 1) @[CSR.scala 648:48]
    node _T_3481 = bits(wdata, 2, 2) @[CSR.scala 648:48]
    node _T_3482 = bits(wdata, 3, 3) @[CSR.scala 648:48]
    node _T_3483 = bits(wdata, 4, 4) @[CSR.scala 648:48]
    skip
    node _T_3485 = bits(wdata, 6, 6) @[CSR.scala 648:48]
    node _T_3486 = bits(wdata, 8, 7) @[CSR.scala 648:48]
    skip
    skip
    node _T_3489 = bits(wdata, 12, 12) @[CSR.scala 648:48]
    skip
    skip
    node _T_3492 = bits(wdata, 59, 59) @[CSR.scala 648:48]
    skip
    skip
    node _T_3494 = and(_T_3492, reg_debug) @[CSR.scala 649:36]
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
    node _T_3495 = and(_T_3494, _T_3489) @[CSR.scala 652:38]
    skip
    skip
    skip
    skip
    skip
    node _GEN_200 = mux(_T_1469, _T_3494, reg_bp_0_control_dmode) @[CSR.scala 647:42]
    skip
    skip
    skip
    skip
    skip
    node _GEN_206 = mux(_T_1469, _T_3495, reg_bp_0_control_action) @[CSR.scala 647:42]
    skip
    skip
    skip
    skip
    skip
    node _GEN_212 = mux(_T_1469, _T_3486, reg_bp_0_control_tmatch) @[CSR.scala 647:42]
    skip
    node _GEN_214 = mux(_T_1469, _T_3485, reg_bp_0_control_m) @[CSR.scala 647:42]
    skip
    skip
    skip
    node _GEN_218 = mux(_T_1469, _T_3483, reg_bp_0_control_s) @[CSR.scala 647:42]
    skip
    node _GEN_220 = mux(_T_1469, _T_3482, reg_bp_0_control_u) @[CSR.scala 647:42]
    skip
    node _GEN_222 = mux(_T_1469, _T_3481, reg_bp_0_control_x) @[CSR.scala 647:42]
    skip
    node _GEN_224 = mux(_T_1469, _T_3480, reg_bp_0_control_w) @[CSR.scala 647:42]
    skip
    node _GEN_226 = mux(_T_1469, _T_3479, reg_bp_0_control_r) @[CSR.scala 647:42]
    skip
    node _reg_bp_reg_tselect_address = bits(wdata, 38, 0) @[CSR.scala 654:55 CSR.scala 654:55]
    skip
    skip
    node _GEN_230 = mux(_T_1471, _reg_bp_reg_tselect_address, reg_bp_0_address) @[CSR.scala 654:42]
    skip
    skip
    skip
    node _GEN_234 = mux(_T_3428, _GEN_200, reg_bp_0_control_dmode) @[CSR.scala 646:45]
    skip
    skip
    skip
    skip
    skip
    node _GEN_240 = mux(_T_3428, _GEN_206, reg_bp_0_control_action) @[CSR.scala 646:45]
    skip
    skip
    skip
    skip
    skip
    node _GEN_246 = mux(_T_3428, _GEN_212, reg_bp_0_control_tmatch) @[CSR.scala 646:45]
    skip
    node _GEN_248 = mux(_T_3428, _GEN_214, reg_bp_0_control_m) @[CSR.scala 646:45]
    skip
    skip
    skip
    node _GEN_252 = mux(_T_3428, _GEN_218, reg_bp_0_control_s) @[CSR.scala 646:45]
    skip
    node _GEN_254 = mux(_T_3428, _GEN_220, reg_bp_0_control_u) @[CSR.scala 646:45]
    skip
    node _GEN_256 = mux(_T_3428, _GEN_222, reg_bp_0_control_x) @[CSR.scala 646:45]
    skip
    node _GEN_258 = mux(_T_3428, _GEN_224, reg_bp_0_control_w) @[CSR.scala 646:45]
    skip
    node _GEN_260 = mux(_T_3428, _GEN_226, reg_bp_0_control_r) @[CSR.scala 646:45]
    skip
    node _GEN_262 = mux(_T_3428, _GEN_230, reg_bp_0_address) @[CSR.scala 646:45]
    skip
    node _GEN_264 = mux(_T_2870, _GEN_104, _GEN_100) @[CSR.scala 536:49]
    node _GEN_265 = mux(_T_2870, _GEN_105, _GEN_101) @[CSR.scala 536:49]
    node _GEN_266 = mux(_T_2870, _GEN_106, reg_mstatus_mprv) @[CSR.scala 536:49]
    node _GEN_267 = mux(_T_2870, _GEN_107, _GEN_102) @[CSR.scala 536:49]
    node _GEN_268 = mux(_T_2870, _GEN_108, reg_mstatus_mxr) @[CSR.scala 536:49]
    node _GEN_269 = mux(_T_2870, _GEN_145, reg_mstatus_pum) @[CSR.scala 536:49]
    node _GEN_270 = mux(_T_2870, _GEN_144, _GEN_96) @[CSR.scala 536:49]
    node _GEN_271 = mux(_T_2870, _GEN_143, _GEN_95) @[CSR.scala 536:49]
    node _GEN_272 = mux(_T_2870, _GEN_142, _GEN_94) @[CSR.scala 536:49]
    node _GEN_273 = mux(_T_2870, _GEN_113, reg_mstatus_tw) @[CSR.scala 536:49]
    node _GEN_274 = mux(_T_2870, _GEN_114, reg_mstatus_tvm) @[CSR.scala 536:49]
    node _GEN_275 = mux(_T_2870, _GEN_115, reg_mstatus_tsr) @[CSR.scala 536:49]
    node _GEN_276 = mux(_T_2870, _GEN_146, reg_mstatus_fs) @[CSR.scala 536:49]
    node _GEN_277 = mux(_T_2870, _GEN_117, reg_misa) @[CSR.scala 536:49]
    skip
    skip
    skip
    node _GEN_281 = mux(_T_2870, _GEN_121, pad(_GEN_71, 64)) @[CSR.scala 536:49]
    skip
    node _GEN_283 = mux(_T_2870, _GEN_123, pad(reg_mtvec, 64)) @[CSR.scala 536:49]
    skip
    skip
    node _GEN_286 = mux(_T_2870, _GEN_126, pad(_T_943, 64)) @[CSR.scala 536:49]
    node _GEN_287 = mux(_T_2870, _GEN_127, _GEN_1) @[CSR.scala 536:49]
    node _GEN_288 = mux(_T_2870, _GEN_128, pad(_T_932, 64)) @[CSR.scala 536:49]
    node _GEN_289 = mux(_T_2870, _GEN_129, _GEN_0) @[CSR.scala 536:49]
    node _GEN_290 = mux(_T_2870, _GEN_132, pad(_GEN_103, 64)) @[CSR.scala 536:49]
    node _GEN_291 = mux(_T_2870, _GEN_133, pad(reg_frm, 64)) @[CSR.scala 536:49]
    node _GEN_292 = mux(_T_2870, _GEN_134, reg_dcsr_halt) @[CSR.scala 536:49]
    node _GEN_293 = mux(_T_2870, _GEN_135, reg_dcsr_step) @[CSR.scala 536:49]
    node _GEN_294 = mux(_T_2870, _GEN_136, reg_dcsr_ebreakm) @[CSR.scala 536:49]
    node _GEN_295 = mux(_T_2870, _GEN_137, reg_dcsr_ebreaks) @[CSR.scala 536:49]
    node _GEN_296 = mux(_T_2870, _GEN_138, reg_dcsr_ebreaku) @[CSR.scala 536:49]
    node _GEN_297 = mux(_T_2870, _GEN_139, _GEN_63) @[CSR.scala 536:49]
    node _GEN_298 = mux(_T_2870, _GEN_140, pad(_GEN_61, 64)) @[CSR.scala 536:49]
    skip
    skip
    skip
    skip
    node _GEN_303 = mux(_T_2870, _GEN_155, pad(_GEN_64, 64)) @[CSR.scala 536:49]
    node _GEN_304 = mux(_T_2870, _GEN_156, pad(reg_stvec, 64)) @[CSR.scala 536:49]
    skip
    skip
    skip
    skip
    node _GEN_309 = mux(_T_2870, _GEN_161, pad(reg_scounteren, 64)) @[CSR.scala 536:49]
    node _GEN_310 = mux(_T_2870, _GEN_162, pad(reg_mcounteren, 64)) @[CSR.scala 536:49]
    skip
    skip
    skip
    node _GEN_314 = mux(_T_2870, _GEN_234, reg_bp_0_control_dmode) @[CSR.scala 536:49]
    skip
    skip
    skip
    skip
    skip
    node _GEN_320 = mux(_T_2870, _GEN_240, reg_bp_0_control_action) @[CSR.scala 536:49]
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
    node _GEN_336 = mux(_T_2870, _GEN_256, reg_bp_0_control_x) @[CSR.scala 536:49]
    skip
    node _GEN_338 = mux(_T_2870, _GEN_258, reg_bp_0_control_w) @[CSR.scala 536:49]
    skip
    node _GEN_340 = mux(_T_2870, _GEN_260, reg_bp_0_control_r) @[CSR.scala 536:49]
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
    io_rw_rdata <= or(_T_2858, _T_2733) @[CSR.scala 529:15]
    io_decode_fp_illegal <= or(_T_1760, _T_1763) @[CSR.scala 421:24]
    skip
    io_decode_read_illegal <= or(_T_2191, _T_2202) @[CSR.scala 423:26]
    io_decode_write_illegal <= eq(_T_2205, UInt<2>("h0")) @[CSR.scala 429:27]
    io_decode_write_flush <= eq(_T_2218, UInt<1>("h0")) @[CSR.scala 430:25]
    io_decode_system_illegal <= or(_T_2240, _T_2244) @[CSR.scala 431:28]
    io_csr_stall <= reg_wfi @[CSR.scala 527:16]
    io_eret <= or(_T_2344, insn_ret) @[CSR.scala 450:11]
    io_singleStep <= and(reg_dcsr_step, _T_1198) @[CSR.scala 451:17]
    io_status_debug <= reg_debug @[CSR.scala 452:13 CSR.scala 454:19]
    io_status_isa <= bits(reg_misa, 31, 0) @[CSR.scala 452:13 CSR.scala 455:17]
    io_status_prv <= reg_mstatus_prv @[CSR.scala 452:13]
    io_status_sd <= or(_T_2337, _T_2340) @[CSR.scala 452:13 CSR.scala 453:16]
    io_status_zero2 <= UInt<27>("h0") @[CSR.scala 452:13]
    io_status_sxl <= UInt<2>("h2") @[CSR.scala 452:13 CSR.scala 457:17]
    io_status_uxl <= UInt<2>("h2") @[CSR.scala 452:13 CSR.scala 456:17]
    io_status_sd_rv32 <= UInt<1>("h0") @[CSR.scala 452:13]
    io_status_zero1 <= UInt<8>("h0") @[CSR.scala 452:13]
    io_status_tsr <= reg_mstatus_tsr @[CSR.scala 452:13]
    io_status_tw <= reg_mstatus_tw @[CSR.scala 452:13]
    io_status_tvm <= reg_mstatus_tvm @[CSR.scala 452:13]
    io_status_mxr <= reg_mstatus_mxr @[CSR.scala 452:13]
    io_status_pum <= reg_mstatus_pum @[CSR.scala 452:13]
    io_status_mprv <= reg_mstatus_mprv @[CSR.scala 452:13]
    io_status_xs <= UInt<2>("h0") @[CSR.scala 452:13]
    io_status_fs <= reg_mstatus_fs @[CSR.scala 452:13]
    io_status_mpp <= reg_mstatus_mpp @[CSR.scala 452:13]
    io_status_hpp <= UInt<2>("h0") @[CSR.scala 452:13]
    io_status_spp <= reg_mstatus_spp @[CSR.scala 452:13]
    io_status_mpie <= reg_mstatus_mpie @[CSR.scala 452:13]
    io_status_hpie <= UInt<1>("h0") @[CSR.scala 452:13]
    io_status_spie <= reg_mstatus_spie @[CSR.scala 452:13]
    io_status_upie <= UInt<1>("h0") @[CSR.scala 452:13]
    io_status_mie <= reg_mstatus_mie @[CSR.scala 452:13]
    io_status_hie <= UInt<1>("h0") @[CSR.scala 452:13]
    io_status_sie <= reg_mstatus_sie @[CSR.scala 452:13]
    io_status_uie <= UInt<1>("h0") @[CSR.scala 452:13]
    io_ptbr_mode <= reg_sptbr_mode @[CSR.scala 449:11]
    skip
    io_ptbr_ppn <= reg_sptbr_ppn @[CSR.scala 449:11]
    io_evec <= mux(insn_ret, _GEN_93, tvec) @[CSR.scala 448:11 CSR.scala 511:15 CSR.scala 515:15 CSR.scala 522:15]
    io_fatc <= and(system_insn, insn_rs2) @[CSR.scala 447:11]
    io_time <= cat(_T_945, _T_942) @[CSR.scala 526:11]
    io_fcsr_rm <= reg_frm @[CSR.scala 531:14]
    io_interrupt <= or(_T_1208, _T_1203) @[CSR.scala 286:16 CSR.scala 292:18]
    io_interrupt_cause <= mux(_T_1208, UInt<64>("h800000000000000d"), interruptCause) @[CSR.scala 287:22 CSR.scala 293:24]
    skip
    skip
    skip
    skip
    io_bp_0_control_action <= reg_bp_0_control_action @[CSR.scala 288:9]
    skip
    skip
    io_bp_0_control_tmatch <= reg_bp_0_control_tmatch @[CSR.scala 288:9]
    io_bp_0_control_m <= reg_bp_0_control_m @[CSR.scala 288:9]
    skip
    io_bp_0_control_s <= reg_bp_0_control_s @[CSR.scala 288:9]
    io_bp_0_control_u <= reg_bp_0_control_u @[CSR.scala 288:9]
    io_bp_0_control_x <= reg_bp_0_control_x @[CSR.scala 288:9]
    io_bp_0_control_w <= reg_bp_0_control_w @[CSR.scala 288:9]
    io_bp_0_control_r <= reg_bp_0_control_r @[CSR.scala 288:9]
    io_bp_0_address <= reg_bp_0_address @[CSR.scala 288:9]
    skip
    skip
    reg_mstatus_prv <= mux(reset, UInt<2>("h3"), _T_467) @[CSR.scala 200:19]
    skip
    skip
    skip
    skip
    skip
    skip
    reg_mstatus_tsr <= mux(reset, UInt<1>("h0"), _GEN_275) @[CSR.scala 553:27]
    reg_mstatus_tw <= mux(reset, UInt<1>("h0"), _GEN_273) @[CSR.scala 551:26]
    reg_mstatus_tvm <= mux(reset, UInt<1>("h0"), _GEN_274) @[CSR.scala 552:27]
    reg_mstatus_mxr <= mux(reset, UInt<1>("h0"), _GEN_268) @[CSR.scala 545:25]
    reg_mstatus_pum <= mux(reset, UInt<1>("h0"), _GEN_269) @[CSR.scala 547:27 CSR.scala 611:25]
    reg_mstatus_mprv <= mux(reset, UInt<1>("h0"), _GEN_266) @[CSR.scala 543:26]
    skip
    reg_mstatus_fs <= mux(reset, UInt<2>("h0"), _GEN_276) @[CSR.scala 557:47 CSR.scala 612:24]
    reg_mstatus_mpp <= mux(reset, UInt<2>("h3"), _GEN_267) @[CSR.scala 499:23 CSR.scala 520:23 CSR.scala 544:25]
    skip
    reg_mstatus_spp <= mux(reset, UInt<1>("h0"), bits(_GEN_270, 0, 0)) @[CSR.scala 491:23 CSR.scala 509:23 CSR.scala 548:27 CSR.scala 610:25]
    reg_mstatus_mpie <= mux(reset, UInt<1>("h0"), _GEN_265) @[CSR.scala 498:24 CSR.scala 519:24 CSR.scala 540:24]
    skip
    reg_mstatus_spie <= mux(reset, UInt<1>("h0"), _GEN_271) @[CSR.scala 490:24 CSR.scala 508:24 CSR.scala 549:28 CSR.scala 609:26]
    skip
    reg_mstatus_mie <= mux(reset, UInt<1>("h0"), _GEN_264) @[CSR.scala 500:23 CSR.scala 517:51 CSR.scala 539:23]
    skip
    reg_mstatus_sie <= mux(reset, UInt<1>("h0"), _GEN_272) @[CSR.scala 492:23 CSR.scala 507:55 CSR.scala 518:73 CSR.scala 550:27 CSR.scala 608:25]
    skip
    skip
    skip
    skip
    skip
    reg_dcsr_ebreakm <= mux(reset, UInt<1>("h0"), _GEN_294) @[CSR.scala 597:26]
    skip
    reg_dcsr_ebreaks <= mux(reset, UInt<1>("h0"), _GEN_295) @[CSR.scala 598:39]
    reg_dcsr_ebreaku <= mux(reset, UInt<1>("h0"), _GEN_296) @[CSR.scala 599:41]
    skip
    skip
    skip
    reg_dcsr_cause <= mux(reset, UInt<3>("h0"), _GEN_62) @[CSR.scala 484:22]
    reg_dcsr_debugint <= mux(reset, UInt<1>("h0"), io_interrupts_debug) @[CSR.scala 660:21]
    skip
    reg_dcsr_halt <= mux(reset, UInt<1>("h0"), _GEN_292) @[CSR.scala 595:23]
    reg_dcsr_step <= mux(reset, UInt<1>("h0"), _GEN_293) @[CSR.scala 596:23]
    reg_dcsr_prv <= mux(reset, UInt<2>("h3"), _GEN_297) @[CSR.scala 485:20 CSR.scala 600:37]
    reg_debug <= mux(reset, UInt<1>("h0"), _GEN_99) @[CSR.scala 482:17 CSR.scala 514:17]
    reg_dpc <= bits(_GEN_298, 39, 0) @[CSR.scala 483:15 CSR.scala 602:52]
    reg_dscratch <= mux(_T_2870, _GEN_141, reg_dscratch) @[CSR.scala 603:57]
    reg_singleStepped <= mux(_T_1201, UInt<1>("h0"), _GEN_38) @[CSR.scala 468:43 CSR.scala 469:45]
    skip
    skip
    reg_bp_0_control_dmode <= mux(reset, UInt<1>("h0"), _GEN_314) @[CSR.scala 650:22 CSR.scala 651:28 CSR.scala 687:17]
    skip
    skip
    reg_bp_0_control_action <= mux(reset, UInt<1>("h0"), _GEN_320) @[CSR.scala 650:22 CSR.scala 652:29 CSR.scala 686:18]
    skip
    skip
    reg_bp_0_control_tmatch <= mux(_T_2870, _GEN_246, reg_bp_0_control_tmatch) @[CSR.scala 650:22]
    reg_bp_0_control_m <= mux(_T_2870, _GEN_248, reg_bp_0_control_m) @[CSR.scala 650:22]
    skip
    reg_bp_0_control_s <= mux(_T_2870, _GEN_252, reg_bp_0_control_s) @[CSR.scala 650:22]
    reg_bp_0_control_u <= mux(_T_2870, _GEN_254, reg_bp_0_control_u) @[CSR.scala 650:22]
    reg_bp_0_control_x <= mux(reset, UInt<1>("h0"), _GEN_336) @[CSR.scala 650:22 CSR.scala 690:13]
    reg_bp_0_control_w <= mux(reset, UInt<1>("h0"), _GEN_338) @[CSR.scala 650:22 CSR.scala 689:13]
    reg_bp_0_control_r <= mux(reset, UInt<1>("h0"), _GEN_340) @[CSR.scala 650:22 CSR.scala 688:13]
    reg_bp_0_address <= mux(_T_2870, _GEN_262, reg_bp_0_address) @[CSR.scala 654:55]
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
    reg_mie <= mux(_T_2870, _GEN_153, reg_mie) @[CSR.scala 572:50 CSR.scala 629:52]
    reg_mideleg <= mux(_T_2870, _GEN_159, reg_mideleg) @[CSR.scala 635:56]
    reg_medeleg <= mux(_T_2870, _GEN_160, reg_medeleg) @[CSR.scala 636:56]
    skip
    reg_mip_meip <= io_interrupts_meip @[CSR.scala 659:11]
    skip
    reg_mip_seip <= io_interrupts_seip @[CSR.scala 659:11]
    skip
    reg_mip_mtip <= io_interrupts_mtip @[CSR.scala 659:11]
    skip
    reg_mip_stip <= mux(_T_2870, _GEN_119, reg_mip_stip) @[CSR.scala 569:22]
    skip
    reg_mip_msip <= io_interrupts_msip @[CSR.scala 659:11]
    skip
    reg_mip_ssip <= mux(_T_2870, _GEN_147, reg_mip_ssip) @[CSR.scala 568:22 CSR.scala 617:22]
    skip
    reg_mepc <= bits(_GEN_281, 39, 0) @[CSR.scala 495:16 CSR.scala 573:51]
    reg_mcause <= mux(_T_2870, _GEN_124, _GEN_72) @[CSR.scala 496:18 CSR.scala 577:53]
    reg_mbadaddr <= mux(_T_2870, _GEN_125, _GEN_73) @[CSR.scala 497:43 CSR.scala 578:55]
    reg_mscratch <= mux(_T_2870, _GEN_122, reg_mscratch) @[CSR.scala 574:55]
    reg_mtvec <= mux(reset, UInt<32>("h0"), bits(_GEN_283, 31, 0)) @[CSR.scala 576:52]
    reg_mcounteren <= bits(_GEN_310, 31, 0) @[CSR.scala 640:61]
    reg_scounteren <= bits(_GEN_309, 31, 0) @[CSR.scala 637:61]
    reg_sepc <= bits(_GEN_303, 39, 0) @[CSR.scala 487:16 CSR.scala 631:53]
    reg_scause <= mux(_T_2870, _GEN_157, _GEN_65) @[CSR.scala 488:18 CSR.scala 633:55]
    reg_sbadaddr <= mux(_T_2870, _GEN_158, _GEN_66) @[CSR.scala 489:43 CSR.scala 634:57]
    reg_sscratch <= mux(_T_2870, _GEN_154, reg_sscratch) @[CSR.scala 630:57]
    reg_stvec <= bits(_GEN_304, 38, 0) @[CSR.scala 632:54]
    reg_sptbr_mode <= mux(_T_2870, _GEN_151, reg_sptbr_mode) @[CSR.scala 622:54 CSR.scala 623:63]
    skip
    reg_sptbr_ppn <= mux(_T_2870, _GEN_152, reg_sptbr_ppn) @[CSR.scala 625:25]
    reg_wfi <= mux(reset, UInt<1>("h0"), _GEN_37) @[CSR.scala 464:29 CSR.scala 465:56]
    reg_fflags <= bits(_GEN_290, 4, 0) @[CSR.scala 533:16 CSR.scala 588:53 CSR.scala 590:53]
    reg_frm <= bits(_GEN_291, 2, 0) @[CSR.scala 589:50 CSR.scala 590:71]
    _T_931 <= mux(reset, UInt<6>("h0"), bits(_GEN_288, 5, 0)) @[Counters.scala 49:9 Counters.scala 67:11]
    _T_934 <= mux(reset, UInt<58>("h0"), _GEN_289) @[Counters.scala 53:38 Counters.scala 68:23]
    _T_942 <= mux(reset, UInt<6>("h0"), bits(_GEN_286, 5, 0)) @[Counters.scala 49:9 Counters.scala 67:11]
    _T_945 <= mux(reset, UInt<58>("h0"), _GEN_287) @[Counters.scala 53:38 Counters.scala 68:23]
    reg_misa <= mux(reset, UInt<64>("h800000000014112d"), _GEN_277) @[CSR.scala 563:16]
    printf(clock, _T_2352, "Assertion failed: these conditions must be mutually exclusive\n    at CSR.scala:462 assert(PopCount(insn_ret :: insn_call :: insn_break :: io.exception :: Nil) <= 1, \"these conditions must be mutually exclusive\")\n") @[CSR.scala 462:9]
    stop(clock, _T_2352, 1) @[CSR.scala 462:9]
    printf(clock, _T_2365, "Assertion failed\n    at CSR.scala:466 assert(!reg_wfi || io.retire === UInt(0))\n") @[CSR.scala 466:9]
    stop(clock, _T_2365, 1) @[CSR.scala 466:9]
    skip
    skip
    printf(clock, _T_2386, "Assertion failed\n    at CSR.scala:471 assert(!reg_singleStepped || io.retire === UInt(0))\n") @[CSR.scala 471:9]
    stop(clock, _T_2386, 1) @[CSR.scala 471:9]

  module BreakpointUnit :
    input io_status_debug : UInt<1>
    input io_status_prv : UInt<2>
    input io_bp_0_control_action : UInt<1>
    input io_bp_0_control_tmatch : UInt<2>
    input io_bp_0_control_m : UInt<1>
    input io_bp_0_control_s : UInt<1>
    input io_bp_0_control_u : UInt<1>
    input io_bp_0_control_x : UInt<1>
    input io_bp_0_control_w : UInt<1>
    input io_bp_0_control_r : UInt<1>
    input io_bp_0_address : UInt<39>
    input io_pc : UInt<39>
    input io_ea : UInt<39>
    output io_xcpt_if : UInt<1>
    output io_xcpt_ld : UInt<1>
    output io_xcpt_st : UInt<1>
    output io_debug_if : UInt<1>
    output io_debug_ld : UInt<1>
    output io_debug_st : UInt<1>
  
    node _T_212 = eq(io_status_debug, UInt<1>("h0")) @[Breakpoint.scala 30:35]
    skip
    skip
    node _T_215 = cat(cat(io_bp_0_control_m, UInt<1>("h0")), cat(io_bp_0_control_s, io_bp_0_control_u)) @[Cat.scala 30:58]
    node _T_216 = dshr(_T_215, io_status_prv) @[Breakpoint.scala 30:68]
    node _T_217 = bits(_T_216, 0, 0) @[Breakpoint.scala 30:68]
    node _T_218 = and(_T_212, _T_217) @[Breakpoint.scala 30:50]
    skip
    node _T_220 = and(_T_218, io_bp_0_control_r) @[Breakpoint.scala 73:22]
    node _T_221 = bits(io_bp_0_control_tmatch, 1, 1) @[Breakpoint.scala 47:23]
    node _T_222 = geq(io_ea, io_bp_0_address) @[Breakpoint.scala 44:8]
    node _T_223 = bits(io_bp_0_control_tmatch, 0, 0) @[Breakpoint.scala 44:36]
    node _T_224 = xor(_T_222, _T_223) @[Breakpoint.scala 44:20]
    node _T_225 = not(io_ea) @[Breakpoint.scala 41:6]
    skip
    node _T_227 = bits(io_bp_0_address, 0, 0) @[Breakpoint.scala 38:83]
    node _T_228 = and(_T_223, _T_227) @[Breakpoint.scala 38:73]
    node _T_229 = bits(io_bp_0_address, 1, 1) @[Breakpoint.scala 38:83]
    node _T_230 = and(_T_228, _T_229) @[Breakpoint.scala 38:73]
    node _T_231 = bits(io_bp_0_address, 2, 2) @[Breakpoint.scala 38:83]
    node _T_232 = and(_T_230, _T_231) @[Breakpoint.scala 38:73]
    skip
    skip
    node _T_235 = cat(cat(_T_232, _T_230), cat(_T_228, _T_223)) @[Cat.scala 30:58]
    node _GEN_6 = pad(_T_235, 39) @[Breakpoint.scala 41:9]
    node _T_236 = or(_T_225, _GEN_6) @[Breakpoint.scala 41:9]
    node _T_237 = not(io_bp_0_address) @[Breakpoint.scala 41:24]
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
    node _T_248 = or(_T_237, _GEN_6) @[Breakpoint.scala 41:33]
    node _T_249 = eq(_T_236, _T_248) @[Breakpoint.scala 41:19]
    node _T_250 = mux(_T_221, _T_224, _T_249) @[Breakpoint.scala 47:8]
    node _T_251 = and(_T_220, _T_250) @[Breakpoint.scala 73:38]
    skip
    node _T_253 = and(_T_218, io_bp_0_control_w) @[Breakpoint.scala 74:22]
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
    node _T_284 = and(_T_253, _T_250) @[Breakpoint.scala 74:38]
    skip
    node _T_286 = and(_T_218, io_bp_0_control_x) @[Breakpoint.scala 75:22]
    skip
    node _T_288 = geq(io_pc, io_bp_0_address) @[Breakpoint.scala 44:8]
    skip
    node _T_290 = xor(_T_288, _T_223) @[Breakpoint.scala 44:20]
    node _T_291 = not(io_pc) @[Breakpoint.scala 41:6]
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
    node _T_302 = or(_T_291, _GEN_6) @[Breakpoint.scala 41:9]
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
    node _T_315 = eq(_T_302, _T_248) @[Breakpoint.scala 41:19]
    node _T_316 = mux(_T_221, _T_290, _T_315) @[Breakpoint.scala 47:8]
    node _T_317 = and(_T_286, _T_316) @[Breakpoint.scala 75:38]
    skip
    skip
    node _T_322 = eq(io_bp_0_control_action, UInt<1>("h0")) @[Breakpoint.scala 78:37]
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
    io_xcpt_if <= and(_T_317, _T_322) @[Breakpoint.scala 64:14 Breakpoint.scala 80:34]
    io_xcpt_ld <= and(_T_251, _T_322) @[Breakpoint.scala 65:14 Breakpoint.scala 78:34]
    io_xcpt_st <= and(_T_284, _T_322) @[Breakpoint.scala 66:14 Breakpoint.scala 79:34]
    io_debug_if <= and(_T_317, io_bp_0_control_action) @[Breakpoint.scala 67:15 Breakpoint.scala 80:69]
    io_debug_ld <= and(_T_251, io_bp_0_control_action) @[Breakpoint.scala 68:15 Breakpoint.scala 78:69]
    io_debug_st <= and(_T_284, io_bp_0_control_action) @[Breakpoint.scala 69:15 Breakpoint.scala 79:69]

  module ALU :
    input io_dw : UInt<1>
    input io_fn : UInt<4>
    input io_in2 : UInt<64>
    input io_in1 : UInt<64>
    output io_out : UInt<64>
    output io_adder_out : UInt<64>
    output io_cmp_out : UInt<1>
  
    node _T_16 = bits(io_fn, 3, 3) @[ALU.scala 41:29]
    node _T_17 = not(io_in2) @[ALU.scala 61:35]
    node in2_inv = mux(_T_16, _T_17, io_in2) @[ALU.scala 61:20]
    node in1_xor_in2 = xor(io_in1, in2_inv) @[ALU.scala 62:28]
    node _T_18 = add(io_in1, in2_inv) @[ALU.scala 63:26]
    node _T_19 = tail(_T_18, 1) @[ALU.scala 63:26]
    skip
    node _GEN_1 = pad(_T_16, 64) @[ALU.scala 63:36]
    node _T_21 = add(_T_19, _GEN_1) @[ALU.scala 63:36]
    skip
    node _T_23 = bits(io_fn, 0, 0) @[ALU.scala 44:35]
    skip
    node _T_26 = eq(_T_16, UInt<1>("h0")) @[ALU.scala 45:26]
    node _T_28 = eq(in1_xor_in2, UInt<64>("h0")) @[ALU.scala 67:35]
    node _T_29 = bits(io_in1, 63, 63) @[ALU.scala 68:15]
    node _T_30 = bits(io_in2, 63, 63) @[ALU.scala 68:34]
    node _T_31 = eq(_T_29, _T_30) @[ALU.scala 68:24]
    node _T_32 = bits(io_adder_out, 63, 63) @[ALU.scala 68:56]
    node _T_33 = bits(io_fn, 1, 1) @[ALU.scala 43:35]
    skip
    skip
    node _T_36 = mux(_T_33, _T_30, _T_29) @[ALU.scala 69:8]
    node _T_37 = mux(_T_31, _T_32, _T_36) @[ALU.scala 68:8]
    node _T_38 = mux(_T_26, _T_28, _T_37) @[ALU.scala 67:8]
    skip
    skip
    node _T_41 = bits(io_in1, 31, 31) @[ALU.scala 76:55]
    node _T_42 = and(_T_16, _T_41) @[ALU.scala 76:46]
    skip
    node _T_46 = mux(_T_42, UInt<32>("hffffffff"), UInt<32>("h0")) @[Bitwise.scala 71:12]
    skip
    node _T_49 = bits(io_in1, 63, 32) @[ALU.scala 77:48]
    node _T_50 = mux(io_dw, _T_49, _T_46) @[ALU.scala 77:24]
    node _T_51 = bits(io_in2, 5, 5) @[ALU.scala 78:29]
    skip
    node _T_54 = and(_T_51, io_dw) @[ALU.scala 78:33]
    node _T_55 = bits(io_in2, 4, 0) @[ALU.scala 78:60]
    node shamt = cat(_T_54, _T_55) @[Cat.scala 30:58]
    node _T_56 = bits(io_in1, 31, 0) @[ALU.scala 79:34]
    node shin_r = cat(_T_50, _T_56) @[Cat.scala 30:58]
    node _T_58 = eq(io_fn, UInt<4>("h5")) @[ALU.scala 81:24]
    node _T_60 = eq(io_fn, UInt<4>("hb")) @[ALU.scala 81:44]
    node _T_61 = or(_T_58, _T_60) @[ALU.scala 81:35]
    skip
    skip
    node _T_66 = shr(shin_r, 32) @[Bitwise.scala 102:21]
    node _T_67 = pad(_T_66, 64) @[Bitwise.scala 102:31]
    node _T_68 = bits(shin_r, 31, 0) @[Bitwise.scala 102:46]
    node _T_69 = shl(_T_68, 32) @[Bitwise.scala 102:65]
    skip
    node _T_71 = and(_T_69, UInt<64>("hffffffff00000000")) @[Bitwise.scala 102:75]
    node _T_72 = or(_T_67, _T_71) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_76 = shr(_T_72, 16) @[Bitwise.scala 102:21]
    node _GEN_2 = pad(_T_76, 64) @[Bitwise.scala 102:31]
    node _T_77 = and(_GEN_2, UInt<64>("hffff0000ffff")) @[Bitwise.scala 102:31]
    node _T_78 = bits(_T_72, 47, 0) @[Bitwise.scala 102:46]
    node _T_79 = shl(_T_78, 16) @[Bitwise.scala 102:65]
    skip
    node _T_81 = and(_T_79, UInt<64>("hffff0000ffff0000")) @[Bitwise.scala 102:75]
    node _T_82 = or(_T_77, _T_81) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_86 = shr(_T_82, 8) @[Bitwise.scala 102:21]
    node _GEN_3 = pad(_T_86, 64) @[Bitwise.scala 102:31]
    node _T_87 = and(_GEN_3, UInt<64>("hff00ff00ff00ff")) @[Bitwise.scala 102:31]
    node _T_88 = bits(_T_82, 55, 0) @[Bitwise.scala 102:46]
    node _T_89 = shl(_T_88, 8) @[Bitwise.scala 102:65]
    skip
    node _T_91 = and(_T_89, UInt<64>("hff00ff00ff00ff00")) @[Bitwise.scala 102:75]
    node _T_92 = or(_T_87, _T_91) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_96 = shr(_T_92, 4) @[Bitwise.scala 102:21]
    node _GEN_4 = pad(_T_96, 64) @[Bitwise.scala 102:31]
    node _T_97 = and(_GEN_4, UInt<64>("hf0f0f0f0f0f0f0f")) @[Bitwise.scala 102:31]
    node _T_98 = bits(_T_92, 59, 0) @[Bitwise.scala 102:46]
    node _T_99 = shl(_T_98, 4) @[Bitwise.scala 102:65]
    skip
    node _T_101 = and(_T_99, UInt<64>("hf0f0f0f0f0f0f0f0")) @[Bitwise.scala 102:75]
    node _T_102 = or(_T_97, _T_101) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_106 = shr(_T_102, 2) @[Bitwise.scala 102:21]
    node _GEN_5 = pad(_T_106, 64) @[Bitwise.scala 102:31]
    node _T_107 = and(_GEN_5, UInt<64>("h3333333333333333")) @[Bitwise.scala 102:31]
    node _T_108 = bits(_T_102, 61, 0) @[Bitwise.scala 102:46]
    node _T_109 = shl(_T_108, 2) @[Bitwise.scala 102:65]
    skip
    node _T_111 = and(_T_109, UInt<64>("hcccccccccccccccc")) @[Bitwise.scala 102:75]
    node _T_112 = or(_T_107, _T_111) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_116 = shr(_T_112, 1) @[Bitwise.scala 102:21]
    node _GEN_6 = pad(_T_116, 64) @[Bitwise.scala 102:31]
    node _T_117 = and(_GEN_6, UInt<64>("h5555555555555555")) @[Bitwise.scala 102:31]
    node _T_118 = bits(_T_112, 62, 0) @[Bitwise.scala 102:46]
    node _T_119 = shl(_T_118, 1) @[Bitwise.scala 102:65]
    skip
    node _T_121 = and(_T_119, UInt<64>("haaaaaaaaaaaaaaaa")) @[Bitwise.scala 102:75]
    node _T_122 = or(_T_117, _T_121) @[Bitwise.scala 102:39]
    node shin = mux(_T_61, shin_r, _T_122) @[ALU.scala 81:17]
    skip
    node _T_124 = bits(shin, 63, 63) @[ALU.scala 82:41]
    node _T_125 = and(_T_16, _T_124) @[ALU.scala 82:35]
    node _T_126 = cat(_T_125, shin) @[Cat.scala 30:58]
    node _T_127 = asSInt(_T_126) @[ALU.scala 82:57]
    node _T_128 = dshr(_T_127, shamt) @[ALU.scala 82:64]
    node shout_r = bits(_T_128, 63, 0) @[ALU.scala 82:73]
    skip
    skip
    node _T_133 = shr(shout_r, 32) @[Bitwise.scala 102:21]
    node _T_134 = pad(_T_133, 64) @[Bitwise.scala 102:31]
    node _T_135 = bits(shout_r, 31, 0) @[Bitwise.scala 102:46]
    node _T_136 = shl(_T_135, 32) @[Bitwise.scala 102:65]
    skip
    node _T_138 = and(_T_136, UInt<64>("hffffffff00000000")) @[Bitwise.scala 102:75]
    node _T_139 = or(_T_134, _T_138) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_143 = shr(_T_139, 16) @[Bitwise.scala 102:21]
    node _GEN_7 = pad(_T_143, 64) @[Bitwise.scala 102:31]
    node _T_144 = and(_GEN_7, UInt<64>("hffff0000ffff")) @[Bitwise.scala 102:31]
    node _T_145 = bits(_T_139, 47, 0) @[Bitwise.scala 102:46]
    node _T_146 = shl(_T_145, 16) @[Bitwise.scala 102:65]
    skip
    node _T_148 = and(_T_146, UInt<64>("hffff0000ffff0000")) @[Bitwise.scala 102:75]
    node _T_149 = or(_T_144, _T_148) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_153 = shr(_T_149, 8) @[Bitwise.scala 102:21]
    node _GEN_8 = pad(_T_153, 64) @[Bitwise.scala 102:31]
    node _T_154 = and(_GEN_8, UInt<64>("hff00ff00ff00ff")) @[Bitwise.scala 102:31]
    node _T_155 = bits(_T_149, 55, 0) @[Bitwise.scala 102:46]
    node _T_156 = shl(_T_155, 8) @[Bitwise.scala 102:65]
    skip
    node _T_158 = and(_T_156, UInt<64>("hff00ff00ff00ff00")) @[Bitwise.scala 102:75]
    node _T_159 = or(_T_154, _T_158) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_163 = shr(_T_159, 4) @[Bitwise.scala 102:21]
    node _GEN_9 = pad(_T_163, 64) @[Bitwise.scala 102:31]
    node _T_164 = and(_GEN_9, UInt<64>("hf0f0f0f0f0f0f0f")) @[Bitwise.scala 102:31]
    node _T_165 = bits(_T_159, 59, 0) @[Bitwise.scala 102:46]
    node _T_166 = shl(_T_165, 4) @[Bitwise.scala 102:65]
    skip
    node _T_168 = and(_T_166, UInt<64>("hf0f0f0f0f0f0f0f0")) @[Bitwise.scala 102:75]
    node _T_169 = or(_T_164, _T_168) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_173 = shr(_T_169, 2) @[Bitwise.scala 102:21]
    node _GEN_10 = pad(_T_173, 64) @[Bitwise.scala 102:31]
    node _T_174 = and(_GEN_10, UInt<64>("h3333333333333333")) @[Bitwise.scala 102:31]
    node _T_175 = bits(_T_169, 61, 0) @[Bitwise.scala 102:46]
    node _T_176 = shl(_T_175, 2) @[Bitwise.scala 102:65]
    skip
    node _T_178 = and(_T_176, UInt<64>("hcccccccccccccccc")) @[Bitwise.scala 102:75]
    node _T_179 = or(_T_174, _T_178) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_183 = shr(_T_179, 1) @[Bitwise.scala 102:21]
    node _GEN_11 = pad(_T_183, 64) @[Bitwise.scala 102:31]
    node _T_184 = and(_GEN_11, UInt<64>("h5555555555555555")) @[Bitwise.scala 102:31]
    node _T_185 = bits(_T_179, 62, 0) @[Bitwise.scala 102:46]
    node _T_186 = shl(_T_185, 1) @[Bitwise.scala 102:65]
    skip
    node _T_188 = and(_T_186, UInt<64>("haaaaaaaaaaaaaaaa")) @[Bitwise.scala 102:75]
    node shout_l = or(_T_184, _T_188) @[Bitwise.scala 102:39]
    skip
    skip
    skip
    node _T_195 = mux(_T_61, shout_r, UInt<64>("h0")) @[ALU.scala 84:18]
    node _T_197 = eq(io_fn, UInt<4>("h1")) @[ALU.scala 85:25]
    node _T_199 = mux(_T_197, shout_l, UInt<64>("h0")) @[ALU.scala 85:18]
    node shout = or(_T_195, _T_199) @[ALU.scala 84:74]
    node _T_201 = eq(io_fn, UInt<4>("h4")) @[ALU.scala 88:25]
    node _T_203 = eq(io_fn, UInt<4>("h6")) @[ALU.scala 88:45]
    node _T_204 = or(_T_201, _T_203) @[ALU.scala 88:36]
    node _T_206 = mux(_T_204, in1_xor_in2, UInt<64>("h0")) @[ALU.scala 88:18]
    skip
    node _T_210 = eq(io_fn, UInt<4>("h7")) @[ALU.scala 89:44]
    node _T_211 = or(_T_203, _T_210) @[ALU.scala 89:35]
    node _T_212 = and(io_in1, io_in2) @[ALU.scala 89:63]
    node _T_214 = mux(_T_211, _T_212, UInt<64>("h0")) @[ALU.scala 89:18]
    node logic = or(_T_206, _T_214) @[ALU.scala 88:78]
    node _T_216 = eq(io_fn, UInt<4>("h2")) @[ALU.scala 42:30]
    node _T_218 = eq(io_fn, UInt<4>("h3")) @[ALU.scala 42:48]
    node _T_219 = or(_T_216, _T_218) @[ALU.scala 42:41]
    node _T_221 = geq(io_fn, UInt<4>("hc")) @[ALU.scala 42:66]
    node _T_222 = or(_T_219, _T_221) @[ALU.scala 42:59]
    node _T_223 = and(_T_222, io_cmp_out) @[ALU.scala 90:35]
    node _GEN_12 = pad(_T_223, 64) @[ALU.scala 90:50]
    node _T_224 = or(_GEN_12, logic) @[ALU.scala 90:50]
    node shift_logic = or(_T_224, shout) @[ALU.scala 90:58]
    node _T_226 = eq(io_fn, UInt<4>("h0")) @[ALU.scala 91:23]
    node _T_228 = eq(io_fn, UInt<4>("ha")) @[ALU.scala 91:43]
    node _T_229 = or(_T_226, _T_228) @[ALU.scala 91:34]
    node out = mux(_T_229, io_adder_out, shift_logic) @[ALU.scala 91:16]
    node _T_231 = eq(io_dw, UInt<1>("h0")) @[ALU.scala 96:17]
    node _T_232 = bits(out, 31, 31) @[ALU.scala 96:56]
    skip
    node _T_236 = mux(_T_232, UInt<32>("hffffffff"), UInt<32>("h0")) @[Bitwise.scala 71:12]
    node _T_237 = bits(out, 31, 0) @[ALU.scala 96:66]
    node _T_238 = cat(_T_236, _T_237) @[Cat.scala 30:58]
    skip
    io_out <= mux(_T_231, _T_238, out) @[ALU.scala 93:10 ALU.scala 96:37]
    io_adder_out <= tail(_T_21, 1) @[ALU.scala 63:16]
    io_cmp_out <= xor(_T_23, _T_38) @[ALU.scala 66:14]

  module MulDiv :
    input clock : Clock
    input reset : UInt<1>
    output io_req_ready : UInt<1>
    input io_req_valid : UInt<1>
    input io_req_bits_fn : UInt<4>
    input io_req_bits_dw : UInt<1>
    input io_req_bits_in1 : UInt<64>
    input io_req_bits_in2 : UInt<64>
    input io_req_bits_tag : UInt<5>
    input io_kill : UInt<1>
    input io_resp_ready : UInt<1>
    output io_resp_valid : UInt<1>
    output io_resp_bits_data : UInt<64>
    output io_resp_bits_tag : UInt<5>
  
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[Multiplier.scala 45:18]
    skip
    reg req_dw : UInt<1>, clock with :
      reset => (UInt<1>("h0"), req_dw) @[Multiplier.scala 47:16]
    skip
    skip
    reg req_tag : UInt<5>, clock with :
      reset => (UInt<1>("h0"), req_tag) @[Multiplier.scala 47:16]
    reg count : UInt<7>, clock with :
      reset => (UInt<1>("h0"), count) @[Multiplier.scala 48:18]
    reg neg_out : UInt<1>, clock with :
      reset => (UInt<1>("h0"), neg_out) @[Multiplier.scala 49:20]
    reg isMul : UInt<1>, clock with :
      reset => (UInt<1>("h0"), isMul) @[Multiplier.scala 50:18]
    reg isHi : UInt<1>, clock with :
      reset => (UInt<1>("h0"), isHi) @[Multiplier.scala 51:17]
    reg divisor : UInt<65>, clock with :
      reset => (UInt<1>("h0"), divisor) @[Multiplier.scala 52:20]
    reg remainder : UInt<130>, clock with :
      reset => (UInt<1>("h0"), remainder) @[Multiplier.scala 53:22]
    node _T_79 = and(io_req_bits_fn, UInt<4>("h4")) @[Decode.scala 13:65]
    node _T_81 = eq(_T_79, UInt<4>("h0")) @[Decode.scala 13:121]
    node _T_83 = and(io_req_bits_fn, UInt<4>("h8")) @[Decode.scala 13:65]
    node _T_85 = eq(_T_83, UInt<4>("h8")) @[Decode.scala 13:121]
    skip
    node cmdMul = or(_T_81, _T_85) @[Decode.scala 14:30]
    node _T_90 = and(io_req_bits_fn, UInt<4>("h5")) @[Decode.scala 13:65]
    node _T_92 = eq(_T_90, UInt<4>("h1")) @[Decode.scala 13:121]
    node _T_94 = and(io_req_bits_fn, UInt<4>("h2")) @[Decode.scala 13:65]
    node _T_96 = eq(_T_94, UInt<4>("h2")) @[Decode.scala 13:121]
    skip
    node _T_99 = or(_T_92, _T_96) @[Decode.scala 14:30]
    node cmdHi = or(_T_99, _T_85) @[Decode.scala 14:30]
    node _T_102 = and(io_req_bits_fn, UInt<4>("h9")) @[Decode.scala 13:65]
    node _T_104 = eq(_T_102, UInt<4>("h0")) @[Decode.scala 13:121]
    node _T_106 = and(io_req_bits_fn, UInt<4>("h3")) @[Decode.scala 13:65]
    node _T_108 = eq(_T_106, UInt<4>("h0")) @[Decode.scala 13:121]
    skip
    node _T_111 = or(_T_104, _T_81) @[Decode.scala 14:30]
    node lhsSigned = or(_T_111, _T_108) @[Decode.scala 14:30]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_118 = eq(io_req_bits_dw, UInt<1>("h0")) @[Multiplier.scala 67:62]
    skip
    node _T_120 = bits(io_req_bits_in1, 31, 31) @[Multiplier.scala 70:38]
    node _T_121 = bits(io_req_bits_in1, 63, 63) @[Multiplier.scala 70:48]
    node _T_122 = mux(_T_118, _T_120, _T_121) @[Multiplier.scala 70:29]
    node lhs_sign = and(lhsSigned, _T_122) @[Multiplier.scala 70:23]
    skip
    node _T_126 = mux(lhs_sign, UInt<32>("hffffffff"), UInt<32>("h0")) @[Bitwise.scala 71:12]
    node _T_127 = bits(io_req_bits_in1, 63, 32) @[Multiplier.scala 71:43]
    node _T_128 = mux(_T_118, _T_126, _T_127) @[Multiplier.scala 71:17]
    node _T_129 = bits(io_req_bits_in1, 31, 0) @[Multiplier.scala 72:15]
    node lhs_in = cat(_T_128, _T_129) @[Cat.scala 30:58]
    skip
    skip
    node _T_134 = bits(io_req_bits_in2, 31, 31) @[Multiplier.scala 70:38]
    node _T_135 = bits(io_req_bits_in2, 63, 63) @[Multiplier.scala 70:48]
    node _T_136 = mux(_T_118, _T_134, _T_135) @[Multiplier.scala 70:29]
    node rhs_sign = and(_T_111, _T_136) @[Multiplier.scala 70:23]
    skip
    node _T_140 = mux(rhs_sign, UInt<32>("hffffffff"), UInt<32>("h0")) @[Bitwise.scala 71:12]
    node _T_141 = bits(io_req_bits_in2, 63, 32) @[Multiplier.scala 71:43]
    node _T_142 = mux(_T_118, _T_140, _T_141) @[Multiplier.scala 71:17]
    node _T_143 = bits(io_req_bits_in2, 31, 0) @[Multiplier.scala 72:15]
    skip
    node _T_144 = bits(remainder, 128, 64) @[Multiplier.scala 77:29]
    node _T_145 = sub(_T_144, divisor) @[Multiplier.scala 77:37]
    node _T_146 = asUInt(_T_145) @[Multiplier.scala 77:37]
    node subtractor = tail(_T_146, 1) @[Multiplier.scala 77:37]
    node _T_147 = bits(remainder, 63, 0) @[Multiplier.scala 78:37]
    node _T_149 = sub(UInt<64>("h0"), _T_147) @[Multiplier.scala 78:27]
    node _T_150 = asUInt(_T_149) @[Multiplier.scala 78:27]
    node negated_remainder = tail(_T_150, 1) @[Multiplier.scala 78:27]
    node _T_151 = eq(state, UInt<3>("h1")) @[Multiplier.scala 80:15]
    node _T_152 = bits(remainder, 63, 63) @[Multiplier.scala 81:20]
    node _T_153 = or(_T_152, isMul) @[Multiplier.scala 81:26]
    node _GEN_0 = mux(_T_153, pad(negated_remainder, 130), remainder) @[Multiplier.scala 81:36]
    node _T_154 = bits(divisor, 63, 63) @[Multiplier.scala 84:18]
    node _T_155 = or(_T_154, isMul) @[Multiplier.scala 84:24]
    node _GEN_1 = mux(_T_155, subtractor, divisor) @[Multiplier.scala 84:34]
    node _GEN_2 = mux(_T_151, _GEN_0, remainder) @[Multiplier.scala 80:33]
    node _GEN_3 = mux(_T_151, _GEN_1, divisor) @[Multiplier.scala 80:33]
    node _GEN_4 = mux(_T_151, UInt<3>("h2"), state) @[Multiplier.scala 80:33]
    node _T_156 = eq(state, UInt<3>("h4")) @[Multiplier.scala 90:15]
    node _GEN_5 = mux(_T_156, pad(negated_remainder, 130), _GEN_2) @[Multiplier.scala 90:33]
    node _GEN_6 = mux(_T_156, UInt<3>("h5"), _GEN_4) @[Multiplier.scala 90:33]
    node _T_157 = eq(state, UInt<3>("h3")) @[Multiplier.scala 94:15]
    node _T_158 = bits(remainder, 128, 65) @[Multiplier.scala 95:27]
    node _T_159 = mux(neg_out, UInt<3>("h4"), UInt<3>("h5")) @[Multiplier.scala 96:17]
    node _GEN_7 = mux(_T_157, pad(_T_158, 130), _GEN_5) @[Multiplier.scala 94:31]
    node _GEN_8 = mux(_T_157, _T_159, _GEN_6) @[Multiplier.scala 94:31]
    node _T_160 = eq(state, UInt<3>("h2")) @[Multiplier.scala 98:15]
    node _T_161 = and(_T_160, isMul) @[Multiplier.scala 98:26]
    node _T_162 = bits(remainder, 129, 65) @[Multiplier.scala 99:31]
    skip
    node _T_164 = cat(_T_162, _T_147) @[Cat.scala 30:58]
    node _T_165 = bits(_T_164, 63, 0) @[Multiplier.scala 100:24]
    node _T_166 = bits(_T_164, 128, 64) @[Multiplier.scala 101:23]
    node _T_167 = asSInt(_T_166) @[Multiplier.scala 101:37]
    node _T_168 = asSInt(divisor) @[Multiplier.scala 102:26]
    node _T_169 = bits(_T_165, 7, 0) @[Multiplier.scala 103:22]
    node _M_170 = asSInt(_T_169) @[Multiplier.scala 103:22]
    node _GEN_34 = pad(_M_170, 65) @[Multiplier.scala 103:43]
    node _T_170 = mul(_T_168, _GEN_34) @[Multiplier.scala 103:43]
    node _GEN_35 = pad(_T_167, 73) @[Multiplier.scala 103:52]
    node _T_171 = add(_T_170, _GEN_35) @[Multiplier.scala 103:52]
    node _T_172 = tail(_T_171, 1) @[Multiplier.scala 103:52]
    node _T_173 = asSInt(_T_172) @[Multiplier.scala 103:52]
    node _T_174 = bits(_T_165, 63, 8) @[Multiplier.scala 104:38]
    node _T_175 = asUInt(_T_173) @[Cat.scala 30:58]
    node _T_176 = cat(_T_175, _T_174) @[Cat.scala 30:58]
    node _T_179 = mul(count, UInt<7>("h8")) @[Multiplier.scala 106:56]
    node _T_180 = bits(_T_179, 5, 0) @[Multiplier.scala 106:72]
    node _T_181 = dshr(SInt<65>("h-10000000000000000"), _T_180) @[Multiplier.scala 106:46]
    node _T_182 = bits(_T_181, 63, 0) @[Multiplier.scala 106:91]
    node _T_185 = neq(count, UInt<7>("h7")) @[Multiplier.scala 107:47]
    skip
    node _T_188 = neq(count, UInt<7>("h0")) @[Multiplier.scala 107:81]
    node _T_189 = and(_T_185, _T_188) @[Multiplier.scala 107:72]
    node _T_191 = eq(isHi, UInt<1>("h0")) @[Multiplier.scala 108:7]
    node _T_192 = and(_T_189, _T_191) @[Multiplier.scala 107:87]
    node _T_193 = not(_T_182) @[Multiplier.scala 108:26]
    node _T_194 = and(_T_165, _T_193) @[Multiplier.scala 108:24]
    node _T_196 = eq(_T_194, UInt<64>("h0")) @[Multiplier.scala 108:37]
    node _T_197 = and(_T_192, _T_196) @[Multiplier.scala 108:13]
    skip
    node _T_201 = sub(UInt<11>("h40"), _T_179) @[Multiplier.scala 109:36]
    node _T_202 = asUInt(_T_201) @[Multiplier.scala 109:36]
    node _T_203 = tail(_T_202, 1) @[Multiplier.scala 109:36]
    node _T_204 = bits(_T_203, 5, 0) @[Multiplier.scala 109:60]
    node _T_205 = dshr(_T_164, _T_204) @[Multiplier.scala 109:27]
    node _T_206 = bits(_T_176, 128, 64) @[Multiplier.scala 110:37]
    node _T_207 = mux(_T_197, _T_205, _T_176) @[Multiplier.scala 110:55]
    node _T_208 = bits(_T_207, 63, 0) @[Multiplier.scala 110:82]
    node _T_209 = cat(_T_206, _T_208) @[Cat.scala 30:58]
    node _T_210 = shr(_T_209, 64) @[Multiplier.scala 111:34]
    node _T_212 = bits(_T_209, 63, 0) @[Multiplier.scala 111:64]
    skip
    node _T_214 = cat(cat(_T_210, UInt<1>("h0")), _T_212) @[Cat.scala 30:58]
    node _T_216 = add(count, UInt<7>("h1")) @[Multiplier.scala 113:20]
    node _T_217 = tail(_T_216, 1) @[Multiplier.scala 113:20]
    node _T_219 = eq(count, UInt<7>("h7")) @[Multiplier.scala 114:25]
    node _T_220 = or(_T_197, _T_219) @[Multiplier.scala 114:16]
    node _T_221 = mux(isHi, UInt<3>("h3"), UInt<3>("h5")) @[Multiplier.scala 115:19]
    node _GEN_9 = mux(_T_220, _T_221, _GEN_8) @[Multiplier.scala 114:51]
    node _GEN_10 = mux(_T_161, _T_214, _GEN_7) @[Multiplier.scala 98:36]
    node _GEN_11 = mux(_T_161, _T_217, count) @[Multiplier.scala 98:36]
    node _GEN_12 = mux(_T_161, _GEN_9, _GEN_8) @[Multiplier.scala 98:36]
    skip
    node _T_224 = eq(isMul, UInt<1>("h0")) @[Multiplier.scala 118:29]
    node _T_225 = and(_T_160, _T_224) @[Multiplier.scala 118:26]
    node _T_226 = bits(subtractor, 64, 64) @[Multiplier.scala 122:28]
    node _T_227 = bits(remainder, 127, 64) @[Multiplier.scala 123:24]
    node _T_228 = bits(subtractor, 63, 0) @[Multiplier.scala 123:45]
    node _T_229 = mux(_T_226, _T_227, _T_228) @[Multiplier.scala 123:14]
    skip
    node _T_232 = eq(_T_226, UInt<1>("h0")) @[Multiplier.scala 123:67]
    skip
    node _T_234 = cat(cat(_T_229, _T_147), _T_232) @[Cat.scala 30:58]
    node _T_236 = eq(count, UInt<7>("h40")) @[Multiplier.scala 127:17]
    skip
    node _T_238 = mux(isHi, UInt<3>("h3"), _T_159) @[Multiplier.scala 128:19]
    node _GEN_13 = mux(_T_236, _T_238, _GEN_12) @[Multiplier.scala 127:38]
    skip
    skip
    node _T_243 = eq(count, UInt<7>("h0")) @[Multiplier.scala 134:24]
    skip
    skip
    node _T_247 = and(_T_243, _T_232) @[Multiplier.scala 134:30]
    node _T_248 = bits(divisor, 63, 0) @[Multiplier.scala 136:36]
    node _T_249 = bits(_T_248, 63, 32) @[CircuitMath.scala 35:17]
    node _T_250 = bits(_T_248, 31, 0) @[CircuitMath.scala 36:17]
    node _T_252 = neq(_T_249, UInt<32>("h0")) @[CircuitMath.scala 37:22]
    node _T_253 = bits(_T_249, 31, 16) @[CircuitMath.scala 35:17]
    node _T_254 = bits(_T_249, 15, 0) @[CircuitMath.scala 36:17]
    node _T_256 = neq(_T_253, UInt<16>("h0")) @[CircuitMath.scala 37:22]
    node _T_257 = bits(_T_253, 15, 8) @[CircuitMath.scala 35:17]
    node _T_258 = bits(_T_253, 7, 0) @[CircuitMath.scala 36:17]
    node _T_260 = neq(_T_257, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_261 = bits(_T_257, 7, 4) @[CircuitMath.scala 35:17]
    node _T_262 = bits(_T_257, 3, 0) @[CircuitMath.scala 36:17]
    node _T_264 = neq(_T_261, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_265 = bits(_T_261, 3, 3) @[CircuitMath.scala 32:12]
    node _T_267 = bits(_T_261, 2, 2) @[CircuitMath.scala 32:12]
    node _T_269 = bits(_T_261, 1, 1) @[CircuitMath.scala 30:8]
    node _T_270 = mux(_T_267, UInt<2>("h2"), pad(_T_269, 2)) @[CircuitMath.scala 32:10]
    node _T_271 = mux(_T_265, UInt<2>("h3"), _T_270) @[CircuitMath.scala 32:10]
    node _T_272 = bits(_T_262, 3, 3) @[CircuitMath.scala 32:12]
    node _T_274 = bits(_T_262, 2, 2) @[CircuitMath.scala 32:12]
    node _T_276 = bits(_T_262, 1, 1) @[CircuitMath.scala 30:8]
    node _T_277 = mux(_T_274, UInt<2>("h2"), pad(_T_276, 2)) @[CircuitMath.scala 32:10]
    node _T_278 = mux(_T_272, UInt<2>("h3"), _T_277) @[CircuitMath.scala 32:10]
    node _T_279 = mux(_T_264, _T_271, _T_278) @[CircuitMath.scala 38:21]
    node _T_280 = cat(_T_264, _T_279) @[Cat.scala 30:58]
    node _T_281 = bits(_T_258, 7, 4) @[CircuitMath.scala 35:17]
    node _T_282 = bits(_T_258, 3, 0) @[CircuitMath.scala 36:17]
    node _T_284 = neq(_T_281, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_285 = bits(_T_281, 3, 3) @[CircuitMath.scala 32:12]
    node _T_287 = bits(_T_281, 2, 2) @[CircuitMath.scala 32:12]
    node _T_289 = bits(_T_281, 1, 1) @[CircuitMath.scala 30:8]
    node _T_290 = mux(_T_287, UInt<2>("h2"), pad(_T_289, 2)) @[CircuitMath.scala 32:10]
    node _T_291 = mux(_T_285, UInt<2>("h3"), _T_290) @[CircuitMath.scala 32:10]
    node _T_292 = bits(_T_282, 3, 3) @[CircuitMath.scala 32:12]
    node _T_294 = bits(_T_282, 2, 2) @[CircuitMath.scala 32:12]
    node _T_296 = bits(_T_282, 1, 1) @[CircuitMath.scala 30:8]
    node _T_297 = mux(_T_294, UInt<2>("h2"), pad(_T_296, 2)) @[CircuitMath.scala 32:10]
    node _T_298 = mux(_T_292, UInt<2>("h3"), _T_297) @[CircuitMath.scala 32:10]
    node _T_299 = mux(_T_284, _T_291, _T_298) @[CircuitMath.scala 38:21]
    node _T_300 = cat(_T_284, _T_299) @[Cat.scala 30:58]
    node _T_301 = mux(_T_260, _T_280, _T_300) @[CircuitMath.scala 38:21]
    node _T_302 = cat(_T_260, _T_301) @[Cat.scala 30:58]
    node _T_303 = bits(_T_254, 15, 8) @[CircuitMath.scala 35:17]
    node _T_304 = bits(_T_254, 7, 0) @[CircuitMath.scala 36:17]
    node _T_306 = neq(_T_303, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_307 = bits(_T_303, 7, 4) @[CircuitMath.scala 35:17]
    node _T_308 = bits(_T_303, 3, 0) @[CircuitMath.scala 36:17]
    node _T_310 = neq(_T_307, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_311 = bits(_T_307, 3, 3) @[CircuitMath.scala 32:12]
    node _T_313 = bits(_T_307, 2, 2) @[CircuitMath.scala 32:12]
    node _T_315 = bits(_T_307, 1, 1) @[CircuitMath.scala 30:8]
    node _T_316 = mux(_T_313, UInt<2>("h2"), pad(_T_315, 2)) @[CircuitMath.scala 32:10]
    node _T_317 = mux(_T_311, UInt<2>("h3"), _T_316) @[CircuitMath.scala 32:10]
    node _T_318 = bits(_T_308, 3, 3) @[CircuitMath.scala 32:12]
    node _T_320 = bits(_T_308, 2, 2) @[CircuitMath.scala 32:12]
    node _T_322 = bits(_T_308, 1, 1) @[CircuitMath.scala 30:8]
    node _T_323 = mux(_T_320, UInt<2>("h2"), pad(_T_322, 2)) @[CircuitMath.scala 32:10]
    node _T_324 = mux(_T_318, UInt<2>("h3"), _T_323) @[CircuitMath.scala 32:10]
    node _T_325 = mux(_T_310, _T_317, _T_324) @[CircuitMath.scala 38:21]
    node _T_326 = cat(_T_310, _T_325) @[Cat.scala 30:58]
    node _T_327 = bits(_T_304, 7, 4) @[CircuitMath.scala 35:17]
    node _T_328 = bits(_T_304, 3, 0) @[CircuitMath.scala 36:17]
    node _T_330 = neq(_T_327, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_331 = bits(_T_327, 3, 3) @[CircuitMath.scala 32:12]
    node _T_333 = bits(_T_327, 2, 2) @[CircuitMath.scala 32:12]
    node _T_335 = bits(_T_327, 1, 1) @[CircuitMath.scala 30:8]
    node _T_336 = mux(_T_333, UInt<2>("h2"), pad(_T_335, 2)) @[CircuitMath.scala 32:10]
    node _T_337 = mux(_T_331, UInt<2>("h3"), _T_336) @[CircuitMath.scala 32:10]
    node _T_338 = bits(_T_328, 3, 3) @[CircuitMath.scala 32:12]
    node _T_340 = bits(_T_328, 2, 2) @[CircuitMath.scala 32:12]
    node _T_342 = bits(_T_328, 1, 1) @[CircuitMath.scala 30:8]
    node _T_343 = mux(_T_340, UInt<2>("h2"), pad(_T_342, 2)) @[CircuitMath.scala 32:10]
    node _T_344 = mux(_T_338, UInt<2>("h3"), _T_343) @[CircuitMath.scala 32:10]
    node _T_345 = mux(_T_330, _T_337, _T_344) @[CircuitMath.scala 38:21]
    node _T_346 = cat(_T_330, _T_345) @[Cat.scala 30:58]
    node _T_347 = mux(_T_306, _T_326, _T_346) @[CircuitMath.scala 38:21]
    node _T_348 = cat(_T_306, _T_347) @[Cat.scala 30:58]
    node _T_349 = mux(_T_256, _T_302, _T_348) @[CircuitMath.scala 38:21]
    node _T_350 = cat(_T_256, _T_349) @[Cat.scala 30:58]
    node _T_351 = bits(_T_250, 31, 16) @[CircuitMath.scala 35:17]
    node _T_352 = bits(_T_250, 15, 0) @[CircuitMath.scala 36:17]
    node _T_354 = neq(_T_351, UInt<16>("h0")) @[CircuitMath.scala 37:22]
    node _T_355 = bits(_T_351, 15, 8) @[CircuitMath.scala 35:17]
    node _T_356 = bits(_T_351, 7, 0) @[CircuitMath.scala 36:17]
    node _T_358 = neq(_T_355, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_359 = bits(_T_355, 7, 4) @[CircuitMath.scala 35:17]
    node _T_360 = bits(_T_355, 3, 0) @[CircuitMath.scala 36:17]
    node _T_362 = neq(_T_359, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_363 = bits(_T_359, 3, 3) @[CircuitMath.scala 32:12]
    node _T_365 = bits(_T_359, 2, 2) @[CircuitMath.scala 32:12]
    node _T_367 = bits(_T_359, 1, 1) @[CircuitMath.scala 30:8]
    node _T_368 = mux(_T_365, UInt<2>("h2"), pad(_T_367, 2)) @[CircuitMath.scala 32:10]
    node _T_369 = mux(_T_363, UInt<2>("h3"), _T_368) @[CircuitMath.scala 32:10]
    node _T_370 = bits(_T_360, 3, 3) @[CircuitMath.scala 32:12]
    node _T_372 = bits(_T_360, 2, 2) @[CircuitMath.scala 32:12]
    node _T_374 = bits(_T_360, 1, 1) @[CircuitMath.scala 30:8]
    node _T_375 = mux(_T_372, UInt<2>("h2"), pad(_T_374, 2)) @[CircuitMath.scala 32:10]
    node _T_376 = mux(_T_370, UInt<2>("h3"), _T_375) @[CircuitMath.scala 32:10]
    node _T_377 = mux(_T_362, _T_369, _T_376) @[CircuitMath.scala 38:21]
    node _T_378 = cat(_T_362, _T_377) @[Cat.scala 30:58]
    node _T_379 = bits(_T_356, 7, 4) @[CircuitMath.scala 35:17]
    node _T_380 = bits(_T_356, 3, 0) @[CircuitMath.scala 36:17]
    node _T_382 = neq(_T_379, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_383 = bits(_T_379, 3, 3) @[CircuitMath.scala 32:12]
    node _T_385 = bits(_T_379, 2, 2) @[CircuitMath.scala 32:12]
    node _T_387 = bits(_T_379, 1, 1) @[CircuitMath.scala 30:8]
    node _T_388 = mux(_T_385, UInt<2>("h2"), pad(_T_387, 2)) @[CircuitMath.scala 32:10]
    node _T_389 = mux(_T_383, UInt<2>("h3"), _T_388) @[CircuitMath.scala 32:10]
    node _T_390 = bits(_T_380, 3, 3) @[CircuitMath.scala 32:12]
    node _T_392 = bits(_T_380, 2, 2) @[CircuitMath.scala 32:12]
    node _T_394 = bits(_T_380, 1, 1) @[CircuitMath.scala 30:8]
    node _T_395 = mux(_T_392, UInt<2>("h2"), pad(_T_394, 2)) @[CircuitMath.scala 32:10]
    node _T_396 = mux(_T_390, UInt<2>("h3"), _T_395) @[CircuitMath.scala 32:10]
    node _T_397 = mux(_T_382, _T_389, _T_396) @[CircuitMath.scala 38:21]
    node _T_398 = cat(_T_382, _T_397) @[Cat.scala 30:58]
    node _T_399 = mux(_T_358, _T_378, _T_398) @[CircuitMath.scala 38:21]
    node _T_400 = cat(_T_358, _T_399) @[Cat.scala 30:58]
    node _T_401 = bits(_T_352, 15, 8) @[CircuitMath.scala 35:17]
    node _T_402 = bits(_T_352, 7, 0) @[CircuitMath.scala 36:17]
    node _T_404 = neq(_T_401, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_405 = bits(_T_401, 7, 4) @[CircuitMath.scala 35:17]
    node _T_406 = bits(_T_401, 3, 0) @[CircuitMath.scala 36:17]
    node _T_408 = neq(_T_405, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_409 = bits(_T_405, 3, 3) @[CircuitMath.scala 32:12]
    node _T_411 = bits(_T_405, 2, 2) @[CircuitMath.scala 32:12]
    node _T_413 = bits(_T_405, 1, 1) @[CircuitMath.scala 30:8]
    node _T_414 = mux(_T_411, UInt<2>("h2"), pad(_T_413, 2)) @[CircuitMath.scala 32:10]
    node _T_415 = mux(_T_409, UInt<2>("h3"), _T_414) @[CircuitMath.scala 32:10]
    node _T_416 = bits(_T_406, 3, 3) @[CircuitMath.scala 32:12]
    node _T_418 = bits(_T_406, 2, 2) @[CircuitMath.scala 32:12]
    node _T_420 = bits(_T_406, 1, 1) @[CircuitMath.scala 30:8]
    node _T_421 = mux(_T_418, UInt<2>("h2"), pad(_T_420, 2)) @[CircuitMath.scala 32:10]
    node _T_422 = mux(_T_416, UInt<2>("h3"), _T_421) @[CircuitMath.scala 32:10]
    node _T_423 = mux(_T_408, _T_415, _T_422) @[CircuitMath.scala 38:21]
    node _T_424 = cat(_T_408, _T_423) @[Cat.scala 30:58]
    node _T_425 = bits(_T_402, 7, 4) @[CircuitMath.scala 35:17]
    node _T_426 = bits(_T_402, 3, 0) @[CircuitMath.scala 36:17]
    node _T_428 = neq(_T_425, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_429 = bits(_T_425, 3, 3) @[CircuitMath.scala 32:12]
    node _T_431 = bits(_T_425, 2, 2) @[CircuitMath.scala 32:12]
    node _T_433 = bits(_T_425, 1, 1) @[CircuitMath.scala 30:8]
    node _T_434 = mux(_T_431, UInt<2>("h2"), pad(_T_433, 2)) @[CircuitMath.scala 32:10]
    node _T_435 = mux(_T_429, UInt<2>("h3"), _T_434) @[CircuitMath.scala 32:10]
    node _T_436 = bits(_T_426, 3, 3) @[CircuitMath.scala 32:12]
    node _T_438 = bits(_T_426, 2, 2) @[CircuitMath.scala 32:12]
    node _T_440 = bits(_T_426, 1, 1) @[CircuitMath.scala 30:8]
    node _T_441 = mux(_T_438, UInt<2>("h2"), pad(_T_440, 2)) @[CircuitMath.scala 32:10]
    node _T_442 = mux(_T_436, UInt<2>("h3"), _T_441) @[CircuitMath.scala 32:10]
    node _T_443 = mux(_T_428, _T_435, _T_442) @[CircuitMath.scala 38:21]
    node _T_444 = cat(_T_428, _T_443) @[Cat.scala 30:58]
    node _T_445 = mux(_T_404, _T_424, _T_444) @[CircuitMath.scala 38:21]
    node _T_446 = cat(_T_404, _T_445) @[Cat.scala 30:58]
    node _T_447 = mux(_T_354, _T_400, _T_446) @[CircuitMath.scala 38:21]
    node _T_448 = cat(_T_354, _T_447) @[Cat.scala 30:58]
    node _T_449 = mux(_T_252, _T_350, _T_448) @[CircuitMath.scala 38:21]
    node _T_450 = cat(_T_252, _T_449) @[Cat.scala 30:58]
    skip
    node _T_452 = bits(_T_147, 63, 32) @[CircuitMath.scala 35:17]
    node _T_453 = bits(_T_147, 31, 0) @[CircuitMath.scala 36:17]
    node _T_455 = neq(_T_452, UInt<32>("h0")) @[CircuitMath.scala 37:22]
    node _T_456 = bits(_T_452, 31, 16) @[CircuitMath.scala 35:17]
    node _T_457 = bits(_T_452, 15, 0) @[CircuitMath.scala 36:17]
    node _T_459 = neq(_T_456, UInt<16>("h0")) @[CircuitMath.scala 37:22]
    node _T_460 = bits(_T_456, 15, 8) @[CircuitMath.scala 35:17]
    node _T_461 = bits(_T_456, 7, 0) @[CircuitMath.scala 36:17]
    node _T_463 = neq(_T_460, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_464 = bits(_T_460, 7, 4) @[CircuitMath.scala 35:17]
    node _T_465 = bits(_T_460, 3, 0) @[CircuitMath.scala 36:17]
    node _T_467 = neq(_T_464, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_468 = bits(_T_464, 3, 3) @[CircuitMath.scala 32:12]
    node _T_470 = bits(_T_464, 2, 2) @[CircuitMath.scala 32:12]
    node _T_472 = bits(_T_464, 1, 1) @[CircuitMath.scala 30:8]
    node _T_473 = mux(_T_470, UInt<2>("h2"), pad(_T_472, 2)) @[CircuitMath.scala 32:10]
    node _T_474 = mux(_T_468, UInt<2>("h3"), _T_473) @[CircuitMath.scala 32:10]
    node _T_475 = bits(_T_465, 3, 3) @[CircuitMath.scala 32:12]
    node _T_477 = bits(_T_465, 2, 2) @[CircuitMath.scala 32:12]
    node _T_479 = bits(_T_465, 1, 1) @[CircuitMath.scala 30:8]
    node _T_480 = mux(_T_477, UInt<2>("h2"), pad(_T_479, 2)) @[CircuitMath.scala 32:10]
    node _T_481 = mux(_T_475, UInt<2>("h3"), _T_480) @[CircuitMath.scala 32:10]
    node _T_482 = mux(_T_467, _T_474, _T_481) @[CircuitMath.scala 38:21]
    node _T_483 = cat(_T_467, _T_482) @[Cat.scala 30:58]
    node _T_484 = bits(_T_461, 7, 4) @[CircuitMath.scala 35:17]
    node _T_485 = bits(_T_461, 3, 0) @[CircuitMath.scala 36:17]
    node _T_487 = neq(_T_484, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_488 = bits(_T_484, 3, 3) @[CircuitMath.scala 32:12]
    node _T_490 = bits(_T_484, 2, 2) @[CircuitMath.scala 32:12]
    node _T_492 = bits(_T_484, 1, 1) @[CircuitMath.scala 30:8]
    node _T_493 = mux(_T_490, UInt<2>("h2"), pad(_T_492, 2)) @[CircuitMath.scala 32:10]
    node _T_494 = mux(_T_488, UInt<2>("h3"), _T_493) @[CircuitMath.scala 32:10]
    node _T_495 = bits(_T_485, 3, 3) @[CircuitMath.scala 32:12]
    node _T_497 = bits(_T_485, 2, 2) @[CircuitMath.scala 32:12]
    node _T_499 = bits(_T_485, 1, 1) @[CircuitMath.scala 30:8]
    node _T_500 = mux(_T_497, UInt<2>("h2"), pad(_T_499, 2)) @[CircuitMath.scala 32:10]
    node _T_501 = mux(_T_495, UInt<2>("h3"), _T_500) @[CircuitMath.scala 32:10]
    node _T_502 = mux(_T_487, _T_494, _T_501) @[CircuitMath.scala 38:21]
    node _T_503 = cat(_T_487, _T_502) @[Cat.scala 30:58]
    node _T_504 = mux(_T_463, _T_483, _T_503) @[CircuitMath.scala 38:21]
    node _T_505 = cat(_T_463, _T_504) @[Cat.scala 30:58]
    node _T_506 = bits(_T_457, 15, 8) @[CircuitMath.scala 35:17]
    node _T_507 = bits(_T_457, 7, 0) @[CircuitMath.scala 36:17]
    node _T_509 = neq(_T_506, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_510 = bits(_T_506, 7, 4) @[CircuitMath.scala 35:17]
    node _T_511 = bits(_T_506, 3, 0) @[CircuitMath.scala 36:17]
    node _T_513 = neq(_T_510, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_514 = bits(_T_510, 3, 3) @[CircuitMath.scala 32:12]
    node _T_516 = bits(_T_510, 2, 2) @[CircuitMath.scala 32:12]
    node _T_518 = bits(_T_510, 1, 1) @[CircuitMath.scala 30:8]
    node _T_519 = mux(_T_516, UInt<2>("h2"), pad(_T_518, 2)) @[CircuitMath.scala 32:10]
    node _T_520 = mux(_T_514, UInt<2>("h3"), _T_519) @[CircuitMath.scala 32:10]
    node _T_521 = bits(_T_511, 3, 3) @[CircuitMath.scala 32:12]
    node _T_523 = bits(_T_511, 2, 2) @[CircuitMath.scala 32:12]
    node _T_525 = bits(_T_511, 1, 1) @[CircuitMath.scala 30:8]
    node _T_526 = mux(_T_523, UInt<2>("h2"), pad(_T_525, 2)) @[CircuitMath.scala 32:10]
    node _T_527 = mux(_T_521, UInt<2>("h3"), _T_526) @[CircuitMath.scala 32:10]
    node _T_528 = mux(_T_513, _T_520, _T_527) @[CircuitMath.scala 38:21]
    node _T_529 = cat(_T_513, _T_528) @[Cat.scala 30:58]
    node _T_530 = bits(_T_507, 7, 4) @[CircuitMath.scala 35:17]
    node _T_531 = bits(_T_507, 3, 0) @[CircuitMath.scala 36:17]
    node _T_533 = neq(_T_530, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_534 = bits(_T_530, 3, 3) @[CircuitMath.scala 32:12]
    node _T_536 = bits(_T_530, 2, 2) @[CircuitMath.scala 32:12]
    node _T_538 = bits(_T_530, 1, 1) @[CircuitMath.scala 30:8]
    node _T_539 = mux(_T_536, UInt<2>("h2"), pad(_T_538, 2)) @[CircuitMath.scala 32:10]
    node _T_540 = mux(_T_534, UInt<2>("h3"), _T_539) @[CircuitMath.scala 32:10]
    node _T_541 = bits(_T_531, 3, 3) @[CircuitMath.scala 32:12]
    node _T_543 = bits(_T_531, 2, 2) @[CircuitMath.scala 32:12]
    node _T_545 = bits(_T_531, 1, 1) @[CircuitMath.scala 30:8]
    node _T_546 = mux(_T_543, UInt<2>("h2"), pad(_T_545, 2)) @[CircuitMath.scala 32:10]
    node _T_547 = mux(_T_541, UInt<2>("h3"), _T_546) @[CircuitMath.scala 32:10]
    node _T_548 = mux(_T_533, _T_540, _T_547) @[CircuitMath.scala 38:21]
    node _T_549 = cat(_T_533, _T_548) @[Cat.scala 30:58]
    node _T_550 = mux(_T_509, _T_529, _T_549) @[CircuitMath.scala 38:21]
    node _T_551 = cat(_T_509, _T_550) @[Cat.scala 30:58]
    node _T_552 = mux(_T_459, _T_505, _T_551) @[CircuitMath.scala 38:21]
    node _T_553 = cat(_T_459, _T_552) @[Cat.scala 30:58]
    node _T_554 = bits(_T_453, 31, 16) @[CircuitMath.scala 35:17]
    node _T_555 = bits(_T_453, 15, 0) @[CircuitMath.scala 36:17]
    node _T_557 = neq(_T_554, UInt<16>("h0")) @[CircuitMath.scala 37:22]
    node _T_558 = bits(_T_554, 15, 8) @[CircuitMath.scala 35:17]
    node _T_559 = bits(_T_554, 7, 0) @[CircuitMath.scala 36:17]
    node _T_561 = neq(_T_558, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_562 = bits(_T_558, 7, 4) @[CircuitMath.scala 35:17]
    node _T_563 = bits(_T_558, 3, 0) @[CircuitMath.scala 36:17]
    node _T_565 = neq(_T_562, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_566 = bits(_T_562, 3, 3) @[CircuitMath.scala 32:12]
    node _T_568 = bits(_T_562, 2, 2) @[CircuitMath.scala 32:12]
    node _T_570 = bits(_T_562, 1, 1) @[CircuitMath.scala 30:8]
    node _T_571 = mux(_T_568, UInt<2>("h2"), pad(_T_570, 2)) @[CircuitMath.scala 32:10]
    node _T_572 = mux(_T_566, UInt<2>("h3"), _T_571) @[CircuitMath.scala 32:10]
    node _T_573 = bits(_T_563, 3, 3) @[CircuitMath.scala 32:12]
    node _T_575 = bits(_T_563, 2, 2) @[CircuitMath.scala 32:12]
    node _T_577 = bits(_T_563, 1, 1) @[CircuitMath.scala 30:8]
    node _T_578 = mux(_T_575, UInt<2>("h2"), pad(_T_577, 2)) @[CircuitMath.scala 32:10]
    node _T_579 = mux(_T_573, UInt<2>("h3"), _T_578) @[CircuitMath.scala 32:10]
    node _T_580 = mux(_T_565, _T_572, _T_579) @[CircuitMath.scala 38:21]
    node _T_581 = cat(_T_565, _T_580) @[Cat.scala 30:58]
    node _T_582 = bits(_T_559, 7, 4) @[CircuitMath.scala 35:17]
    node _T_583 = bits(_T_559, 3, 0) @[CircuitMath.scala 36:17]
    node _T_585 = neq(_T_582, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_586 = bits(_T_582, 3, 3) @[CircuitMath.scala 32:12]
    node _T_588 = bits(_T_582, 2, 2) @[CircuitMath.scala 32:12]
    node _T_590 = bits(_T_582, 1, 1) @[CircuitMath.scala 30:8]
    node _T_591 = mux(_T_588, UInt<2>("h2"), pad(_T_590, 2)) @[CircuitMath.scala 32:10]
    node _T_592 = mux(_T_586, UInt<2>("h3"), _T_591) @[CircuitMath.scala 32:10]
    node _T_593 = bits(_T_583, 3, 3) @[CircuitMath.scala 32:12]
    node _T_595 = bits(_T_583, 2, 2) @[CircuitMath.scala 32:12]
    node _T_597 = bits(_T_583, 1, 1) @[CircuitMath.scala 30:8]
    node _T_598 = mux(_T_595, UInt<2>("h2"), pad(_T_597, 2)) @[CircuitMath.scala 32:10]
    node _T_599 = mux(_T_593, UInt<2>("h3"), _T_598) @[CircuitMath.scala 32:10]
    node _T_600 = mux(_T_585, _T_592, _T_599) @[CircuitMath.scala 38:21]
    node _T_601 = cat(_T_585, _T_600) @[Cat.scala 30:58]
    node _T_602 = mux(_T_561, _T_581, _T_601) @[CircuitMath.scala 38:21]
    node _T_603 = cat(_T_561, _T_602) @[Cat.scala 30:58]
    node _T_604 = bits(_T_555, 15, 8) @[CircuitMath.scala 35:17]
    node _T_605 = bits(_T_555, 7, 0) @[CircuitMath.scala 36:17]
    node _T_607 = neq(_T_604, UInt<8>("h0")) @[CircuitMath.scala 37:22]
    node _T_608 = bits(_T_604, 7, 4) @[CircuitMath.scala 35:17]
    node _T_609 = bits(_T_604, 3, 0) @[CircuitMath.scala 36:17]
    node _T_611 = neq(_T_608, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_612 = bits(_T_608, 3, 3) @[CircuitMath.scala 32:12]
    node _T_614 = bits(_T_608, 2, 2) @[CircuitMath.scala 32:12]
    node _T_616 = bits(_T_608, 1, 1) @[CircuitMath.scala 30:8]
    node _T_617 = mux(_T_614, UInt<2>("h2"), pad(_T_616, 2)) @[CircuitMath.scala 32:10]
    node _T_618 = mux(_T_612, UInt<2>("h3"), _T_617) @[CircuitMath.scala 32:10]
    node _T_619 = bits(_T_609, 3, 3) @[CircuitMath.scala 32:12]
    node _T_621 = bits(_T_609, 2, 2) @[CircuitMath.scala 32:12]
    node _T_623 = bits(_T_609, 1, 1) @[CircuitMath.scala 30:8]
    node _T_624 = mux(_T_621, UInt<2>("h2"), pad(_T_623, 2)) @[CircuitMath.scala 32:10]
    node _T_625 = mux(_T_619, UInt<2>("h3"), _T_624) @[CircuitMath.scala 32:10]
    node _T_626 = mux(_T_611, _T_618, _T_625) @[CircuitMath.scala 38:21]
    node _T_627 = cat(_T_611, _T_626) @[Cat.scala 30:58]
    node _T_628 = bits(_T_605, 7, 4) @[CircuitMath.scala 35:17]
    node _T_629 = bits(_T_605, 3, 0) @[CircuitMath.scala 36:17]
    node _T_631 = neq(_T_628, UInt<4>("h0")) @[CircuitMath.scala 37:22]
    node _T_632 = bits(_T_628, 3, 3) @[CircuitMath.scala 32:12]
    node _T_634 = bits(_T_628, 2, 2) @[CircuitMath.scala 32:12]
    node _T_636 = bits(_T_628, 1, 1) @[CircuitMath.scala 30:8]
    node _T_637 = mux(_T_634, UInt<2>("h2"), pad(_T_636, 2)) @[CircuitMath.scala 32:10]
    node _T_638 = mux(_T_632, UInt<2>("h3"), _T_637) @[CircuitMath.scala 32:10]
    node _T_639 = bits(_T_629, 3, 3) @[CircuitMath.scala 32:12]
    node _T_641 = bits(_T_629, 2, 2) @[CircuitMath.scala 32:12]
    node _T_643 = bits(_T_629, 1, 1) @[CircuitMath.scala 30:8]
    node _T_644 = mux(_T_641, UInt<2>("h2"), pad(_T_643, 2)) @[CircuitMath.scala 32:10]
    node _T_645 = mux(_T_639, UInt<2>("h3"), _T_644) @[CircuitMath.scala 32:10]
    node _T_646 = mux(_T_631, _T_638, _T_645) @[CircuitMath.scala 38:21]
    node _T_647 = cat(_T_631, _T_646) @[Cat.scala 30:58]
    node _T_648 = mux(_T_607, _T_627, _T_647) @[CircuitMath.scala 38:21]
    node _T_649 = cat(_T_607, _T_648) @[Cat.scala 30:58]
    node _T_650 = mux(_T_557, _T_603, _T_649) @[CircuitMath.scala 38:21]
    node _T_651 = cat(_T_557, _T_650) @[Cat.scala 30:58]
    node _T_652 = mux(_T_455, _T_553, _T_651) @[CircuitMath.scala 38:21]
    node _T_653 = cat(_T_455, _T_652) @[Cat.scala 30:58]
    node _T_655 = add(UInt<6>("h3f"), _T_450) @[Multiplier.scala 138:31]
    node _T_656 = tail(_T_655, 1) @[Multiplier.scala 138:31]
    node _T_657 = sub(_T_656, _T_653) @[Multiplier.scala 138:44]
    node _T_658 = asUInt(_T_657) @[Multiplier.scala 138:44]
    node _T_659 = tail(_T_658, 1) @[Multiplier.scala 138:44]
    node _T_660 = gt(_T_450, _T_653) @[Multiplier.scala 139:33]
    skip
    node _T_664 = eq(_T_247, UInt<1>("h0")) @[Multiplier.scala 140:33]
    node _T_665 = and(_T_243, _T_664) @[Multiplier.scala 140:30]
    node _T_667 = geq(_T_659, UInt<6>("h1")) @[Multiplier.scala 140:53]
    node _T_668 = or(_T_667, _T_660) @[Multiplier.scala 140:70]
    node _T_669 = and(_T_665, _T_668) @[Multiplier.scala 140:41]
    node _T_671 = mux(_T_660, UInt<6>("h3f"), _T_659) @[Multiplier.scala 142:22]
    skip
    skip
    skip
    node _GEN_36 = pad(_T_147, 127) @[Multiplier.scala 144:39]
    node _T_675 = dshlw(_GEN_36, _T_671) @[Multiplier.scala 144:39]
    node _GEN_14 = mux(_T_669, pad(_T_675, 129), _T_234) @[Multiplier.scala 141:19]
    node _GEN_15 = mux(_T_669, pad(_T_671, 7), _T_217) @[Multiplier.scala 141:19]
    skip
    node _T_678 = and(_T_247, _T_191) @[Multiplier.scala 148:18]
    node _GEN_16 = mux(_T_678, UInt<1>("h0"), neg_out) @[Multiplier.scala 148:28]
    node _GEN_17 = mux(_T_225, pad(_GEN_14, 130), _GEN_10) @[Multiplier.scala 118:37]
    node _GEN_18 = mux(_T_225, _GEN_13, _GEN_12) @[Multiplier.scala 118:37]
    node _GEN_19 = mux(_T_225, _GEN_15, _GEN_11) @[Multiplier.scala 118:37]
    node _GEN_20 = mux(_T_225, _GEN_16, neg_out) @[Multiplier.scala 118:37]
    node _T_680 = and(io_resp_ready, io_resp_valid) @[Decoupled.scala 30:37]
    node _T_681 = or(_T_680, io_kill) @[Multiplier.scala 150:24]
    node _GEN_21 = mux(_T_681, UInt<3>("h0"), _GEN_18) @[Multiplier.scala 150:36]
    node _T_682 = and(io_req_ready, io_req_valid) @[Decoupled.scala 30:37]
    node _T_684 = eq(cmdMul, UInt<1>("h0")) @[Multiplier.scala 154:42]
    node _T_685 = and(rhs_sign, _T_684) @[Multiplier.scala 154:39]
    node _T_686 = or(lhs_sign, _T_685) @[Multiplier.scala 154:27]
    node _T_687 = mux(_T_686, UInt<3>("h1"), UInt<3>("h2")) @[Multiplier.scala 154:17]
    skip
    node _T_691 = neq(lhs_sign, rhs_sign) @[Multiplier.scala 158:57]
    node _T_692 = mux(cmdHi, lhs_sign, _T_691) @[Multiplier.scala 158:30]
    node _T_693 = and(_T_684, _T_692) @[Multiplier.scala 158:24]
    node _T_694 = cat(rhs_sign, cat(_T_142, _T_143)) @[Cat.scala 30:58]
    node _GEN_22 = mux(_T_682, _T_687, _GEN_21) @[Multiplier.scala 153:24]
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
    node _T_697 = eq(req_dw, UInt<1>("h0")) @[Multiplier.scala 67:62]
    skip
    node _T_699 = bits(remainder, 31, 31) @[Multiplier.scala 165:67]
    skip
    node _T_703 = mux(_T_699, UInt<32>("hffffffff"), UInt<32>("h0")) @[Bitwise.scala 71:12]
    node _T_704 = bits(remainder, 31, 0) @[Multiplier.scala 165:86]
    node _T_705 = cat(_T_703, _T_704) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    io_req_ready <= eq(state, UInt<3>("h0")) @[Multiplier.scala 167:16]
    io_resp_valid <= eq(state, UInt<3>("h5")) @[Multiplier.scala 166:17]
    io_resp_bits_data <= mux(_T_697, _T_705, _T_147) @[Multiplier.scala 165:21]
    io_resp_bits_tag <= req_tag @[Multiplier.scala 164:16]
    state <= mux(reset, UInt<3>("h0"), _GEN_22) @[Multiplier.scala 87:11 Multiplier.scala 92:11 Multiplier.scala 96:11 Multiplier.scala 115:13 Multiplier.scala 128:13 Multiplier.scala 151:11 Multiplier.scala 154:11]
    skip
    req_dw <= mux(_T_682, io_req_bits_dw, req_dw) @[Multiplier.scala 161:9]
    skip
    skip
    req_tag <= mux(_T_682, io_req_bits_tag, req_tag) @[Multiplier.scala 161:9]
    count <= mux(_T_682, UInt<7>("h0"), _GEN_19) @[Multiplier.scala 113:11 Multiplier.scala 132:11 Multiplier.scala 145:15 Multiplier.scala 157:11]
    neg_out <= mux(_T_682, _T_693, _GEN_20) @[Multiplier.scala 148:38 Multiplier.scala 158:13]
    isMul <= mux(_T_682, cmdMul, isMul) @[Multiplier.scala 155:11]
    isHi <= mux(_T_682, cmdHi, isHi) @[Multiplier.scala 156:10]
    divisor <= mux(_T_682, _T_694, _GEN_3) @[Multiplier.scala 85:15 Multiplier.scala 159:13]
    remainder <= mux(_T_682, pad(lhs_in, 130), _GEN_17) @[Multiplier.scala 82:17 Multiplier.scala 91:15 Multiplier.scala 95:15 Multiplier.scala 111:15 Multiplier.scala 126:15 Multiplier.scala 144:19 Multiplier.scala 160:15]

  module RVCExpander :
    input io_in : UInt<32>
    output io_out_bits : UInt<32>
    output io_out_rd : UInt<5>
    output io_out_rs1 : UInt<5>
    output io_out_rs2 : UInt<5>
    output io_out_rs3 : UInt<5>
    output io_rvc : UInt<1>
  
    node _T_12 = bits(io_in, 1, 0) @[RVC.scala 162:20]
    skip
    node _T_15 = bits(io_in, 12, 5) @[RVC.scala 53:22]
    node _T_17 = neq(_T_15, UInt<8>("h0")) @[RVC.scala 53:29]
    node _T_20 = mux(_T_17, UInt<7>("h13"), UInt<7>("h1f")) @[RVC.scala 53:20]
    node _T_21 = bits(io_in, 10, 7) @[RVC.scala 34:26]
    node _T_22 = bits(io_in, 12, 11) @[RVC.scala 34:35]
    node _T_23 = bits(io_in, 5, 5) @[RVC.scala 34:45]
    node _T_24 = bits(io_in, 6, 6) @[RVC.scala 34:51]
    skip
    skip
    skip
    skip
    node _T_33 = bits(io_in, 4, 2) @[RVC.scala 31:30]
    node _T_34 = cat(UInt<2>("h1"), _T_33) @[Cat.scala 30:58]
    skip
    skip
    skip
    node _T_38 = cat(cat(cat(cat(cat(cat(_T_21, _T_22), _T_23), cat(_T_24, UInt<2>("h0"))), UInt<5>("h2")), UInt<3>("h0")), cat(cat(UInt<2>("h1"), _T_33), _T_20)) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    node _T_46 = bits(io_in, 31, 27) @[RVC.scala 20:101]
    node _T_59 = bits(io_in, 6, 5) @[RVC.scala 36:20]
    node _T_60 = bits(io_in, 12, 10) @[RVC.scala 36:28]
    skip
    node _T_63 = cat(cat(_T_59, _T_60), UInt<3>("h0")) @[Cat.scala 30:58]
    node _T_65 = bits(io_in, 9, 7) @[RVC.scala 30:30]
    node _T_66 = cat(UInt<2>("h1"), _T_65) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    node _T_75 = cat(cat(cat(cat(cat(_T_59, _T_60), UInt<3>("h0")), cat(UInt<2>("h1"), _T_65)), UInt<3>("h3")), cat(cat(UInt<2>("h1"), _T_33), UInt<7>("h7"))) @[Cat.scala 30:58]
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
    node _T_104 = cat(cat(_T_23, _T_60), cat(_T_24, UInt<2>("h0"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_116 = cat(cat(cat(cat(cat(_T_23, _T_60), cat(_T_24, UInt<2>("h0"))), cat(UInt<2>("h1"), _T_65)), UInt<3>("h2")), cat(cat(UInt<2>("h1"), _T_33), UInt<7>("h3"))) @[Cat.scala 30:58]
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
    node _T_155 = cat(cat(cat(cat(cat(_T_59, _T_60), UInt<3>("h0")), cat(UInt<2>("h1"), _T_65)), UInt<3>("h3")), cat(cat(UInt<2>("h1"), _T_33), UInt<7>("h3"))) @[Cat.scala 30:58]
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
    node _T_185 = shr(_T_104, 5) @[RVC.scala 63:32]
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
    node _T_200 = bits(_T_104, 4, 0) @[RVC.scala 63:66]
    skip
    skip
    skip
    skip
    node _T_206 = cat(cat(cat(_T_185, cat(UInt<2>("h1"), _T_33)), cat(UInt<2>("h1"), _T_65)), cat(cat(UInt<3>("h2"), _T_200), UInt<7>("h2f"))) @[Cat.scala 30:58]
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
    node _T_234 = shr(_T_63, 5) @[RVC.scala 66:30]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_247 = bits(_T_63, 4, 0) @[RVC.scala 66:64]
    skip
    skip
    skip
    skip
    node _T_253 = cat(cat(cat(_T_234, cat(UInt<2>("h1"), _T_33)), cat(UInt<2>("h1"), _T_65)), cat(cat(UInt<3>("h3"), _T_247), UInt<7>("h27"))) @[Cat.scala 30:58]
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
    node _T_304 = cat(cat(cat(_T_185, cat(UInt<2>("h1"), _T_33)), cat(UInt<2>("h1"), _T_65)), cat(cat(UInt<3>("h2"), _T_200), UInt<7>("h23"))) @[Cat.scala 30:58]
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
    node _T_351 = cat(cat(cat(_T_234, cat(UInt<2>("h1"), _T_33)), cat(UInt<2>("h1"), _T_65)), cat(cat(UInt<3>("h3"), _T_247), UInt<7>("h23"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_374 = bits(io_in, 12, 12) @[RVC.scala 43:30]
    skip
    node _T_378 = mux(_T_374, UInt<7>("h7f"), UInt<7>("h0")) @[Bitwise.scala 71:12]
    node _T_379 = bits(io_in, 6, 2) @[RVC.scala 43:38]
    node _T_380 = cat(_T_378, _T_379) @[Cat.scala 30:58]
    node _T_381 = bits(io_in, 11, 7) @[RVC.scala 33:13]
    skip
    skip
    skip
    skip
    node _T_388 = cat(cat(cat(cat(_T_378, _T_379), _T_381), UInt<3>("h0")), cat(_T_381, UInt<7>("h13"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_409 = neq(_T_381, UInt<5>("h0")) @[RVC.scala 77:24]
    node _T_412 = mux(_T_409, UInt<7>("h1b"), UInt<7>("h1f")) @[RVC.scala 77:20]
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
    node _T_426 = cat(cat(cat(cat(_T_378, _T_379), _T_381), UInt<3>("h0")), cat(_T_381, _T_412)) @[Cat.scala 30:58]
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
    node _T_459 = cat(cat(cat(cat(_T_378, _T_379), UInt<5>("h0")), UInt<3>("h0")), cat(_T_381, UInt<7>("h13"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_486 = neq(_T_380, UInt<12>("h0")) @[RVC.scala 90:29]
    node _T_489 = mux(_T_486, UInt<7>("h37"), UInt<7>("h3f")) @[RVC.scala 90:20]
    skip
    skip
    node _T_494 = mux(_T_374, UInt<15>("h7fff"), UInt<15>("h0")) @[Bitwise.scala 71:12]
    skip
    skip
    node _T_498 = cat(cat(_T_494, _T_379), UInt<12>("h0")) @[Cat.scala 30:58]
    node _T_499 = bits(_T_498, 31, 12) @[RVC.scala 91:31]
    skip
    skip
    node _T_502 = cat(cat(_T_499, _T_381), _T_489) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_523 = eq(_T_381, UInt<5>("h0")) @[RVC.scala 92:14]
    skip
    node _T_526 = eq(_T_381, UInt<5>("h2")) @[RVC.scala 92:27]
    node _T_527 = or(_T_523, _T_526) @[RVC.scala 92:21]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_539 = mux(_T_486, UInt<7>("h13"), UInt<7>("h1f")) @[RVC.scala 86:20]
    skip
    skip
    node _T_544 = mux(_T_374, UInt<3>("h7"), UInt<3>("h0")) @[Bitwise.scala 71:12]
    node _T_545 = bits(io_in, 4, 3) @[RVC.scala 42:42]
    skip
    node _T_547 = bits(io_in, 2, 2) @[RVC.scala 42:56]
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
    node _T_561 = cat(cat(cat(cat(cat(cat(_T_544, _T_545), _T_23), cat(cat(_T_547, _T_24), UInt<4>("h0"))), _T_381), UInt<3>("h0")), cat(_T_381, _T_539)) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_580_bits = mux(_T_527, _T_561, _T_502) @[RVC.scala 92:10]
    skip
    skip
    node _T_580_rd = mux(_T_527, _T_381, _T_381) @[RVC.scala 92:10]
    skip
    skip
    skip
    skip
    skip
    node _T_580_rs2 = mux(_T_527, _T_34, _T_34) @[RVC.scala 92:10]
    skip
    skip
    node _T_580_rs3 = mux(_T_527, _T_46, _T_46) @[RVC.scala 92:10]
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
    node _T_600 = cat(cat(cat(cat(_T_374, _T_379), cat(UInt<2>("h1"), _T_65)), UInt<3>("h5")), cat(cat(UInt<2>("h1"), _T_65), UInt<7>("h13"))) @[Cat.scala 30:58]
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
    node _GEN_0 = pad(_T_600, 31) @[RVC.scala 99:23]
    node _T_617 = or(_GEN_0, UInt<31>("h40000000")) @[RVC.scala 99:23]
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
    node _T_636 = cat(cat(cat(cat(_T_378, _T_379), cat(UInt<2>("h1"), _T_65)), UInt<3>("h7")), cat(cat(UInt<2>("h1"), _T_65), UInt<7>("h13"))) @[Cat.scala 30:58]
    skip
    skip
    node _T_647 = cat(_T_374, _T_59) @[Cat.scala 30:58]
    node _T_649 = and(_T_647, UInt<3>("h3")) @[Package.scala 18:26]
    node _T_651 = geq(_T_647, UInt<3>("h4")) @[Package.scala 19:17]
    node _T_653 = and(_T_649, UInt<3>("h1")) @[Package.scala 18:26]
    node _T_655 = geq(_T_649, UInt<3>("h2")) @[Package.scala 19:17]
    skip
    node _T_659 = geq(_T_653, UInt<3>("h1")) @[Package.scala 19:17]
    node _T_660 = mux(_T_659, UInt<2>("h3"), UInt<2>("h2")) @[Package.scala 19:12]
    skip
    skip
    skip
    node _T_666 = mux(_T_655, _T_660, UInt<2>("h0")) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    node _T_675 = mux(_T_659, UInt<3>("h7"), UInt<3>("h6")) @[Package.scala 19:12]
    skip
    skip
    node _T_680 = mux(_T_659, UInt<3>("h4"), UInt<3>("h0")) @[Package.scala 19:12]
    node _T_681 = mux(_T_655, _T_675, _T_680) @[Package.scala 19:12]
    node _T_682 = mux(_T_651, pad(_T_666, 3), _T_681) @[Package.scala 19:12]
    skip
    node _T_685 = eq(_T_59, UInt<2>("h0")) @[RVC.scala 103:30]
    node _T_688 = mux(_T_685, UInt<31>("h40000000"), UInt<31>("h0")) @[RVC.scala 103:22]
    skip
    node _T_692 = mux(_T_374, UInt<7>("h3b"), UInt<7>("h33")) @[RVC.scala 104:22]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_705 = cat(cat(cat(cat(UInt<2>("h1"), _T_33), cat(UInt<2>("h1"), _T_65)), _T_682), cat(cat(UInt<2>("h1"), _T_65), _T_692)) @[Cat.scala 30:58]
    node _GEN_1 = pad(_T_705, 31) @[RVC.scala 105:43]
    node _T_706 = or(_GEN_1, _T_688) @[RVC.scala 105:43]
    node _T_707 = bits(io_in, 11, 10) @[RVC.scala 107:42]
    node _T_709 = and(_T_707, UInt<2>("h1")) @[Package.scala 18:26]
    node _T_711 = geq(_T_707, UInt<2>("h2")) @[Package.scala 19:17]
    skip
    node _T_715 = geq(_T_709, UInt<2>("h1")) @[Package.scala 19:17]
    node _T_716 = mux(_T_715, pad(_T_706, 32), _T_636) @[Package.scala 19:12]
    skip
    skip
    node _T_721 = mux(_T_715, _T_617, pad(_T_600, 31)) @[Package.scala 19:12]
    node _T_722 = mux(_T_711, _T_716, pad(_T_721, 32)) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_749 = mux(_T_374, UInt<10>("h3ff"), UInt<10>("h0")) @[Bitwise.scala 71:12]
    node _T_750 = bits(io_in, 8, 8) @[RVC.scala 44:36]
    node _T_751 = bits(io_in, 10, 9) @[RVC.scala 44:42]
    skip
    node _T_753 = bits(io_in, 7, 7) @[RVC.scala 44:57]
    skip
    node _T_755 = bits(io_in, 11, 11) @[RVC.scala 44:69]
    node _T_756 = bits(io_in, 5, 3) @[RVC.scala 44:76]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_765 = cat(cat(cat(cat(_T_749, _T_750), _T_751), cat(_T_24, _T_753)), cat(cat(_T_547, _T_755), cat(_T_756, UInt<1>("h0")))) @[Cat.scala 30:58]
    node _T_766 = bits(_T_765, 20, 20) @[RVC.scala 94:26]
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
    node _T_788 = bits(_T_765, 10, 1) @[RVC.scala 94:36]
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
    node _T_810 = bits(_T_765, 11, 11) @[RVC.scala 94:48]
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
    node _T_832 = bits(_T_765, 19, 12) @[RVC.scala 94:58]
    skip
    skip
    skip
    skip
    node _T_839 = cat(cat(cat(_T_766, _T_788), _T_810), cat(cat(_T_832, UInt<5>("h0")), UInt<7>("h6f"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_864 = mux(_T_374, UInt<5>("h1f"), UInt<5>("h0")) @[Bitwise.scala 71:12]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_874 = cat(cat(cat(_T_864, _T_59), _T_547), cat(cat(_T_707, _T_545), UInt<1>("h0"))) @[Cat.scala 30:58]
    node _T_875 = bits(_T_874, 12, 12) @[RVC.scala 95:29]
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
    node _T_891 = bits(_T_874, 10, 5) @[RVC.scala 95:39]
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
    node _T_912 = bits(_T_874, 4, 1) @[RVC.scala 95:72]
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
    node _T_928 = bits(_T_874, 11, 11) @[RVC.scala 95:83]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_936 = cat(cat(cat(_T_875, _T_891), cat(UInt<5>("h0"), cat(UInt<2>("h1"), _T_65))), cat(cat(UInt<3>("h0"), _T_912), cat(_T_928, UInt<7>("h63")))) @[Cat.scala 30:58]
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
    node _T_1033 = cat(cat(cat(_T_875, _T_891), cat(UInt<5>("h0"), cat(UInt<2>("h1"), _T_65))), cat(cat(UInt<3>("h1"), _T_912), cat(_T_928, UInt<7>("h63")))) @[Cat.scala 30:58]
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
    node _T_1062 = cat(cat(cat(cat(_T_374, _T_379), _T_381), UInt<3>("h1")), cat(_T_381, UInt<7>("h13"))) @[Cat.scala 30:58]
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
    node _T_1093 = cat(cat(cat(cat(cat(_T_33, _T_374), cat(_T_59, UInt<3>("h0"))), UInt<5>("h2")), UInt<3>("h3")), cat(_T_381, UInt<7>("h7"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    node _T_1110 = bits(io_in, 3, 2) @[RVC.scala 37:22]
    skip
    node _T_1112 = bits(io_in, 6, 4) @[RVC.scala 37:37]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1124 = cat(cat(cat(cat(cat(_T_1110, _T_374), cat(_T_1112, UInt<2>("h0"))), UInt<5>("h2")), UInt<3>("h2")), cat(_T_381, UInt<7>("h3"))) @[Cat.scala 30:58]
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
    node _T_1155 = cat(cat(cat(cat(cat(_T_33, _T_374), cat(_T_59, UInt<3>("h0"))), UInt<5>("h2")), UInt<3>("h3")), cat(_T_381, UInt<7>("h3"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1180 = cat(cat(cat(_T_379, UInt<5>("h0")), UInt<3>("h0")), cat(_T_381, UInt<7>("h33"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1205 = cat(cat(cat(_T_379, _T_381), UInt<3>("h0")), cat(_T_381, UInt<7>("h33"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1230 = cat(cat(cat(_T_379, _T_381), UInt<3>("h0")), UInt<12>("h67")) @[Cat.scala 30:58]
    node _T_1231 = shr(_T_1230, 7) @[RVC.scala 132:29]
    node _T_1233 = cat(_T_1231, UInt<7>("h1f")) @[Cat.scala 30:58]
    skip
    skip
    node _T_1237 = mux(_T_409, _T_1230, _T_1233) @[RVC.scala 133:33]
    skip
    skip
    skip
    skip
    node _T_1256 = neq(_T_379, UInt<5>("h0")) @[RVC.scala 134:27]
    node _T_1191_bits = pad(_T_1180, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1248_bits = pad(_T_1237, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1257_bits = mux(_T_1256, _T_1191_bits, _T_1248_bits) @[RVC.scala 134:22]
    skip
    skip
    node _T_1257_rd = mux(_T_1256, _T_381, UInt<5>("h0")) @[RVC.scala 134:22]
    skip
    skip
    node _T_1257_rs1 = mux(_T_1256, UInt<5>("h0"), _T_381) @[RVC.scala 134:22]
    skip
    skip
    node _T_1257_rs2 = mux(_T_1256, _T_379, _T_379) @[RVC.scala 134:22]
    skip
    skip
    node _T_1257_rs3 = mux(_T_1256, _T_46, _T_46) @[RVC.scala 134:22]
    skip
    skip
    skip
    skip
    skip
    node _T_1271 = cat(cat(cat(_T_379, _T_381), UInt<3>("h0")), UInt<12>("he7")) @[Cat.scala 30:58]
    skip
    node _T_1274 = cat(_T_1231, UInt<7>("h73")) @[Cat.scala 30:58]
    node _T_1276 = or(_T_1274, UInt<25>("h100000")) @[RVC.scala 136:47]
    skip
    skip
    node _T_1280 = mux(_T_409, _T_1271, _T_1276) @[RVC.scala 137:33]
    skip
    skip
    skip
    skip
    skip
    node _T_1216_bits = pad(_T_1205, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1291_bits = pad(_T_1280, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1300_bits = mux(_T_1256, _T_1216_bits, _T_1291_bits) @[RVC.scala 138:25]
    skip
    skip
    node _T_1300_rd = mux(_T_1256, _T_381, UInt<5>("h1")) @[RVC.scala 138:25]
    skip
    skip
    node _T_1300_rs1 = mux(_T_1256, _T_381, _T_381) @[RVC.scala 138:25]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1307_bits = mux(_T_374, _T_1300_bits, _T_1257_bits) @[RVC.scala 139:10]
    node _T_1307_rd = mux(_T_374, _T_1300_rd, _T_1257_rd) @[RVC.scala 139:10]
    node _T_1307_rs1 = mux(_T_374, _T_1300_rs1, _T_1257_rs1) @[RVC.scala 139:10]
    node _T_1307_rs2 = mux(_T_374, _T_1257_rs2, _T_1257_rs2) @[RVC.scala 139:10]
    node _T_1307_rs3 = mux(_T_374, _T_1257_rs3, _T_1257_rs3) @[RVC.scala 139:10]
    skip
    skip
    skip
    node _T_1317 = cat(cat(_T_65, _T_60), UInt<3>("h0")) @[Cat.scala 30:58]
    node _T_1318 = shr(_T_1317, 5) @[RVC.scala 123:34]
    skip
    skip
    skip
    skip
    skip
    node _T_1327 = bits(_T_1317, 4, 0) @[RVC.scala 123:67]
    skip
    skip
    skip
    skip
    node _T_1333 = cat(cat(cat(_T_1318, _T_379), UInt<5>("h2")), cat(cat(UInt<3>("h3"), _T_1327), UInt<7>("h27"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    node _T_1350 = bits(io_in, 8, 7) @[RVC.scala 39:22]
    node _T_1351 = bits(io_in, 12, 9) @[RVC.scala 39:30]
    skip
    node _T_1354 = cat(cat(_T_1350, _T_1351), UInt<2>("h0")) @[Cat.scala 30:58]
    node _T_1355 = shr(_T_1354, 5) @[RVC.scala 122:33]
    skip
    skip
    skip
    skip
    skip
    node _T_1364 = bits(_T_1354, 4, 0) @[RVC.scala 122:66]
    skip
    skip
    skip
    skip
    node _T_1370 = cat(cat(cat(_T_1355, _T_379), UInt<5>("h2")), cat(cat(UInt<3>("h2"), _T_1364), UInt<7>("h23"))) @[Cat.scala 30:58]
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
    node _T_1407 = cat(cat(cat(_T_1318, _T_379), UInt<5>("h2")), cat(cat(UInt<3>("h3"), _T_1327), UInt<7>("h23"))) @[Cat.scala 30:58]
    skip
    skip
    skip
    skip
    node _T_1425 = bits(io_in, 19, 15) @[RVC.scala 20:57]
    node _T_1426 = bits(io_in, 24, 20) @[RVC.scala 20:79]
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
    node _T_1553 = bits(io_in, 15, 13) @[RVC.scala 150:20]
    node _T_1554 = cat(_T_12, _T_1553) @[Cat.scala 30:58]
    node _T_1556 = and(_T_1554, UInt<5>("hf")) @[Package.scala 18:26]
    node _T_1558 = geq(_T_1554, UInt<5>("h10")) @[Package.scala 19:17]
    node _T_1560 = and(_T_1556, UInt<5>("h7")) @[Package.scala 18:26]
    node _T_1562 = geq(_T_1556, UInt<5>("h8")) @[Package.scala 19:17]
    node _T_1564 = and(_T_1560, UInt<5>("h3")) @[Package.scala 18:26]
    node _T_1566 = geq(_T_1560, UInt<5>("h4")) @[Package.scala 19:17]
    node _T_1568 = and(_T_1564, UInt<5>("h1")) @[Package.scala 18:26]
    node _T_1570 = geq(_T_1564, UInt<5>("h2")) @[Package.scala 19:17]
    skip
    node _T_1574 = geq(_T_1568, UInt<5>("h1")) @[Package.scala 19:17]
    skip
    skip
    skip
    skip
    skip
    node _T_1575_rd = mux(_T_1574, _T_381, _T_381) @[Package.scala 19:12]
    skip
    skip
    node _T_1575_rs1 = mux(_T_1574, _T_1425, _T_1425) @[Package.scala 19:12]
    skip
    skip
    node _T_1575_rs2 = mux(_T_1574, _T_1426, _T_1426) @[Package.scala 19:12]
    skip
    skip
    node _T_1575_rs3 = mux(_T_1574, _T_46, _T_46) @[Package.scala 19:12]
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
    node _T_1591_rd = mux(_T_1570, _T_1575_rd, _T_1575_rd) @[Package.scala 19:12]
    node _T_1591_rs1 = mux(_T_1570, _T_1575_rs1, _T_1575_rs1) @[Package.scala 19:12]
    node _T_1591_rs2 = mux(_T_1570, _T_1575_rs2, _T_1575_rs2) @[Package.scala 19:12]
    node _T_1591_rs3 = mux(_T_1570, _T_1575_rs3, _T_1575_rs3) @[Package.scala 19:12]
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
    node _T_1627_rd = mux(_T_1566, _T_1591_rd, _T_1591_rd) @[Package.scala 19:12]
    node _T_1627_rs1 = mux(_T_1566, _T_1591_rs1, _T_1591_rs1) @[Package.scala 19:12]
    node _T_1627_rs2 = mux(_T_1566, _T_1591_rs2, _T_1591_rs2) @[Package.scala 19:12]
    node _T_1627_rs3 = mux(_T_1566, _T_1591_rs3, _T_1591_rs3) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1418_bits = pad(_T_1407, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1381_bits = pad(_T_1370, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1645_bits = mux(_T_1574, _T_1418_bits, _T_1381_bits) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1645_rs2 = mux(_T_1574, _T_379, _T_379) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    node _T_1344_bits = pad(_T_1333, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1655_bits = mux(_T_1574, _T_1344_bits, _T_1307_bits) @[Package.scala 19:12]
    skip
    node _T_1655_rd = mux(_T_1574, _T_381, _T_1307_rd) @[Package.scala 19:12]
    skip
    node _T_1655_rs1 = mux(_T_1574, UInt<5>("h2"), _T_1307_rs1) @[Package.scala 19:12]
    skip
    node _T_1655_rs2 = mux(_T_1574, _T_379, _T_1307_rs2) @[Package.scala 19:12]
    skip
    node _T_1655_rs3 = mux(_T_1574, _T_46, _T_1307_rs3) @[Package.scala 19:12]
    node _T_1661_bits = mux(_T_1570, _T_1645_bits, _T_1655_bits) @[Package.scala 19:12]
    node _T_1661_rd = mux(_T_1570, _T_1575_rd, _T_1655_rd) @[Package.scala 19:12]
    node _T_1661_rs1 = mux(_T_1570, UInt<5>("h2"), _T_1655_rs1) @[Package.scala 19:12]
    node _T_1661_rs2 = mux(_T_1570, _T_1645_rs2, _T_1655_rs2) @[Package.scala 19:12]
    node _T_1661_rs3 = mux(_T_1570, _T_1575_rs3, _T_1655_rs3) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    node _T_1166_bits = pad(_T_1155, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1135_bits = pad(_T_1124, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1675_bits = mux(_T_1574, _T_1166_bits, _T_1135_bits) @[Package.scala 19:12]
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
    node _T_1104_bits = pad(_T_1093, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1073_bits = pad(_T_1062, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1685_bits = mux(_T_1574, _T_1104_bits, _T_1073_bits) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    node _T_1685_rs1 = mux(_T_1574, UInt<5>("h2"), _T_381) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1691_bits = mux(_T_1570, _T_1675_bits, _T_1685_bits) @[Package.scala 19:12]
    skip
    node _T_1691_rs1 = mux(_T_1570, UInt<5>("h2"), _T_1685_rs1) @[Package.scala 19:12]
    node _T_1691_rs2 = mux(_T_1570, _T_1645_rs2, _T_1645_rs2) @[Package.scala 19:12]
    skip
    node _T_1697_bits = mux(_T_1566, _T_1661_bits, _T_1691_bits) @[Package.scala 19:12]
    node _T_1697_rd = mux(_T_1566, _T_1661_rd, _T_1591_rd) @[Package.scala 19:12]
    node _T_1697_rs1 = mux(_T_1566, _T_1661_rs1, _T_1691_rs1) @[Package.scala 19:12]
    node _T_1697_rs2 = mux(_T_1566, _T_1661_rs2, _T_1691_rs2) @[Package.scala 19:12]
    node _T_1697_rs3 = mux(_T_1566, _T_1661_rs3, _T_1591_rs3) @[Package.scala 19:12]
    node _T_1703_bits = mux(_T_1562, io_in, _T_1697_bits) @[Package.scala 19:12]
    node _T_1703_rd = mux(_T_1562, _T_1627_rd, _T_1697_rd) @[Package.scala 19:12]
    node _T_1703_rs1 = mux(_T_1562, _T_1627_rs1, _T_1697_rs1) @[Package.scala 19:12]
    node _T_1703_rs2 = mux(_T_1562, _T_1627_rs2, _T_1697_rs2) @[Package.scala 19:12]
    node _T_1703_rs3 = mux(_T_1562, _T_1627_rs3, _T_1697_rs3) @[Package.scala 19:12]
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
    node _T_1725_bits = mux(_T_1574, _T_1033, _T_936) @[Package.scala 19:12]
    skip
    skip
    node _T_1725_rd = mux(_T_1574, UInt<5>("h0"), _T_66) @[Package.scala 19:12]
    skip
    skip
    node _T_1725_rs1 = mux(_T_1574, _T_66, _T_66) @[Package.scala 19:12]
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
    node _T_1735_bits = mux(_T_1574, _T_839, _T_722) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1735_rs2 = mux(_T_1574, _T_34, _T_34) @[Package.scala 19:12]
    skip
    skip
    skip
    node _T_1741_bits = mux(_T_1570, _T_1725_bits, _T_1735_bits) @[Package.scala 19:12]
    node _T_1741_rd = mux(_T_1570, _T_1725_rd, _T_1725_rd) @[Package.scala 19:12]
    node _T_1741_rs1 = mux(_T_1570, _T_1725_rs1, _T_1725_rs1) @[Package.scala 19:12]
    node _T_1741_rs2 = mux(_T_1570, UInt<5>("h0"), _T_1735_rs2) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1755_bits = mux(_T_1574, _T_580_bits, _T_459) @[Package.scala 19:12]
    skip
    node _T_1755_rd = mux(_T_1574, _T_580_rd, _T_381) @[Package.scala 19:12]
    skip
    node _T_1755_rs1 = mux(_T_1574, _T_580_rd, UInt<5>("h0")) @[Package.scala 19:12]
    skip
    node _T_1755_rs2 = mux(_T_1574, _T_580_rs2, _T_34) @[Package.scala 19:12]
    skip
    node _T_1755_rs3 = mux(_T_1574, _T_580_rs3, _T_46) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    node _T_1765_bits = mux(_T_1574, _T_426, _T_388) @[Package.scala 19:12]
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
    node _T_1771_bits = mux(_T_1570, _T_1755_bits, _T_1765_bits) @[Package.scala 19:12]
    node _T_1771_rd = mux(_T_1570, _T_1755_rd, _T_1575_rd) @[Package.scala 19:12]
    node _T_1771_rs1 = mux(_T_1570, _T_1755_rs1, _T_1575_rd) @[Package.scala 19:12]
    node _T_1771_rs2 = mux(_T_1570, _T_1755_rs2, _T_1735_rs2) @[Package.scala 19:12]
    node _T_1771_rs3 = mux(_T_1570, _T_1755_rs3, _T_1575_rs3) @[Package.scala 19:12]
    node _T_1777_bits = mux(_T_1566, _T_1741_bits, _T_1771_bits) @[Package.scala 19:12]
    node _T_1777_rd = mux(_T_1566, _T_1741_rd, _T_1771_rd) @[Package.scala 19:12]
    node _T_1777_rs1 = mux(_T_1566, _T_1741_rs1, _T_1771_rs1) @[Package.scala 19:12]
    node _T_1777_rs2 = mux(_T_1566, _T_1741_rs2, _T_1771_rs2) @[Package.scala 19:12]
    node _T_1777_rs3 = mux(_T_1566, _T_1591_rs3, _T_1771_rs3) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_368_bits = pad(_T_351, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_321_bits = pad(_T_304, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1795_bits = mux(_T_1574, _T_368_bits, _T_321_bits) @[Package.scala 19:12]
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
    node _T_270_bits = pad(_T_253, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_223_bits = pad(_T_206, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1805_bits = mux(_T_1574, _T_270_bits, _T_223_bits) @[Package.scala 19:12]
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
    node _T_1811_bits = mux(_T_1570, _T_1795_bits, _T_1805_bits) @[Package.scala 19:12]
    node _T_1811_rd = mux(_T_1570, _T_1735_rs2, _T_1735_rs2) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_172_bits = pad(_T_155, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_133_bits = pad(_T_116, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1825_bits = mux(_T_1574, _T_172_bits, _T_133_bits) @[Package.scala 19:12]
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
    node _T_92_bits = pad(_T_75, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_53_bits = pad(_T_38, 32) @[RVC.scala 21:19 RVC.scala 22:14]
    node _T_1835_bits = mux(_T_1574, _T_92_bits, _T_53_bits) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    node _T_1835_rs1 = mux(_T_1574, _T_66, UInt<5>("h2")) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    skip
    node _T_1841_bits = mux(_T_1570, _T_1825_bits, _T_1835_bits) @[Package.scala 19:12]
    skip
    node _T_1841_rs1 = mux(_T_1570, _T_1725_rs1, _T_1835_rs1) @[Package.scala 19:12]
    skip
    skip
    node _T_1847_bits = mux(_T_1566, _T_1811_bits, _T_1841_bits) @[Package.scala 19:12]
    node _T_1847_rd = mux(_T_1566, _T_1811_rd, _T_1811_rd) @[Package.scala 19:12]
    node _T_1847_rs1 = mux(_T_1566, _T_1741_rs1, _T_1841_rs1) @[Package.scala 19:12]
    skip
    skip
    node _T_1853_bits = mux(_T_1562, _T_1777_bits, _T_1847_bits) @[Package.scala 19:12]
    node _T_1853_rd = mux(_T_1562, _T_1777_rd, _T_1847_rd) @[Package.scala 19:12]
    node _T_1853_rs1 = mux(_T_1562, _T_1777_rs1, _T_1847_rs1) @[Package.scala 19:12]
    node _T_1853_rs2 = mux(_T_1562, _T_1777_rs2, _T_1847_rd) @[Package.scala 19:12]
    node _T_1853_rs3 = mux(_T_1562, _T_1777_rs3, _T_1627_rs3) @[Package.scala 19:12]
    skip
    skip
    skip
    skip
    skip
    io_out_bits <= mux(_T_1558, _T_1703_bits, _T_1853_bits) @[RVC.scala 163:12]
    io_out_rd <= mux(_T_1558, _T_1703_rd, _T_1853_rd) @[RVC.scala 163:12]
    io_out_rs1 <= mux(_T_1558, _T_1703_rs1, _T_1853_rs1) @[RVC.scala 163:12]
    io_out_rs2 <= mux(_T_1558, _T_1703_rs2, _T_1853_rs2) @[RVC.scala 163:12]
    io_out_rs3 <= mux(_T_1558, _T_1703_rs3, _T_1853_rs3) @[RVC.scala 163:12]
    io_rvc <= neq(_T_12, UInt<2>("h3")) @[RVC.scala 162:12]
