parameter address_bus_width = 6'd32; 
parameter data_bus_width = 6'd32; 

import uvm_pkg::*;
`include "uvm_macros.svh"

// interface
interface APB_if #(ADDR_BUS_WIDTH = address_bus_width, DATA_BUS_WIDTH = data_bus_width) (input bit PCLK);
    logic                          PRESETn;
    logic                          PSEL;
    logic                          PWRITE;
    logic                          PENABLE; 
    logic [ADDR_BUS_WIDTH - 1 : 0] PADDR; 
    logic [DATA_BUS_WIDTH - 1 : 0] PWDATA; 
    logic [DATA_BUS_WIDTH - 1 : 0] PRDATA;
    logic                          PREADY;
    logic                          PSLVERR;

    clocking cb_APB_master @(posedge PCLK);
        output PRESETn;  
        output PSEL;
        output PWRITE;
        output PENABLE;
        output PADDR; 
        output PWDATA; 
        output PRDATA;
        output PREADY;
        output PSLVERR;
    endclocking : cb_APB_master
    
    modport APB_master(clocking cb_APB_master);
    
    modport APB_monitor(
        input  PCLK,
        input  PRESETn,  
        input  PSEL,
        input  PWRITE,
        input  PENABLE,
        input  PADDR, 
        input  PWDATA, 
        input  PRDATA,
        input  PREADY,
        input  PSLVERR
    );
endinterface : APB_if

// monitor interface
interface APB_if_monitor (APB_if.APB_monitor apb_if_monitor);
    property PSEL_PRESETn_SIGNAL_VALID(signal);
        @(posedge apb_if_monitor.PCLK)
        !$isunknown(signal);
    endproperty : PSEL_PRESETn_SIGNAL_VALID 
    
    PSEL_VALID   : assert property(PSEL_PRESETn_SIGNAL_VALID(apb_if_monitor.PSEL)); 
    PRESETn_VALID: assert property(PSEL_PRESETn_SIGNAL_VALID(apb_if_monitor.PRESETn)); 
    
    property PADDR_PWRITE_PENABLE_SIGNAL_VALID(signal);
        @(posedge apb_if_monitor.PCLK)
        apb_if_monitor.PSEL |-> !$isunknown(signal);
    endproperty : PADDR_PWRITE_PENABLE_SIGNAL_VALID 
    
    PADDR_VALID  : assert property(PADDR_PWRITE_PENABLE_SIGNAL_VALID(apb_if_monitor.PADDR));
    PWRITE_VALID : assert property(PADDR_PWRITE_PENABLE_SIGNAL_VALID(apb_if_monitor.PWRITE));
    PENABLE_VALID: assert property(PADDR_PWRITE_PENABLE_SIGNAL_VALID(apb_if_monitor.PENABLE));
    
    property PWDATA_SIGNAL_VALID; 
        @(posedge apb_if_monitor.PCLK)
        (apb_if_monitor.PSEL && apb_if_monitor.PWRITE) |-> !$isunknown(apb_if_monitor.PWDATA); 
    endproperty : PWDATA_SIGNAL_VALID 
    
    PWDATA_VALID: assert property(PWDATA_SIGNAL_VALID);
    
    property PREADY_PSLVERR_SIGNAL_VALID(signal);
        @(posedge apb_if_monitor.PCLK)
      ($rose(apb_if_monitor.PENABLE)) |->!$isunknown(signal)[*1 : $] ##1 $fell(apb_if_monitor.PENABLE);
    endproperty : PREADY_PSLVERR_SIGNAL_VALID 
    
    PREADY_VALID : assert property(PREADY_PSLVERR_SIGNAL_VALID(apb_if_monitor.PREADY));
    PSLVERR_VALID: assert property(PREADY_PSLVERR_SIGNAL_VALID(apb_if_monitor.PSLVERR)); 
    
    property PRDATA_SIGNAL_VALID;
        @(posedge apb_if_monitor.PCLK)
        ($rose(apb_if_monitor.PENABLE && !apb_if_monitor.PWRITE && apb_if_monitor.PREADY)) |-> 
        !$isunknown(apb_if_monitor.PRDATA)[*1:$] ##1 $fell(apb_if_monitor.PENABLE);
    endproperty: PRDATA_SIGNAL_VALID
     
    PRDATA_VALID: assert property(PRDATA_SIGNAL_VALID);
    
    property PENABLE_SIGNAL_DEASSERT;
        @(posedge apb_if_monitor.PCLK)
      $rose(apb_if_monitor.PENABLE && apb_if_monitor.PREADY) |=> !apb_if_monitor.PENABLE;
    endproperty : PENABLE_SIGNAL_DEASSERT
    
    PENABLE_DEASSERT: assert property(PENABLE_SIGNAL_DEASSERT);
    
    property PSEL_SIGNAL_2_PENABLE_SIGNAL;
        @(posedge apb_if_monitor.PCLK)
        $rose(apb_if_monitor.PSEL) |=> apb_if_monitor.PENABLE;
    endproperty : PSEL_SIGNAL_2_PENABLE_SIGNAL
    
    PSEL_2_PENABLE: assert property(PSEL_SIGNAL_2_PENABLE_SIGNAL);
    
    property PSEL_ASSERT_SIGNAL_STABLE(signal);
        @(posedge apb_if_monitor.PCLK)
        $rose(apb_if_monitor.PSEL) |=> $stable(signal)[*1 : $] ##1 $fell(apb_if_monitor.PENABLE);
    endproperty : PSEL_ASSERT_SIGNAL_STABLE 
    
    PSEL_STABLE  : assert property(PSEL_ASSERT_SIGNAL_STABLE(apb_if_monitor.PSEL));
    PWRITE_STABLE: assert property(PSEL_ASSERT_SIGNAL_STABLE(apb_if_monitor.PWRITE));
    PADDR_STABLE : assert property(PSEL_ASSERT_SIGNAL_STABLE(apb_if_monitor.PADDR));
    PWDATA_STABLE: assert property(PSEL_ASSERT_SIGNAL_STABLE(apb_if_monitor.PWDATA && apb_if_monitor.PWRITE));
endinterface : APB_if_monitor

// transaction
class APB_transaction extends uvm_sequence_item;
    rand logic                             PWRITE;
    rand logic                             PENABLE;
    rand logic                             PSEL;
    rand logic                             PRESETn;
    rand logic [address_bus_width - 1 : 0] PADDR; 
    rand logic [data_bus_width    - 1 : 0] PWDATA; 
    rand logic [data_bus_width    - 1 : 0] PRDATA; 
    rand logic                             PREADY;
    rand logic                             PSLVERR;
    
    function new(string name = "apb_transaction");
        super.new(name);
    endfunction

    `uvm_object_utils_begin(APB_transaction)
        `uvm_field_int(PWRITE , UVM_ALL_ON)
        `uvm_field_int(PENABLE, UVM_ALL_ON)
        `uvm_field_int(PSEL   , UVM_ALL_ON)
        `uvm_field_int(PRESETn, UVM_ALL_ON)
        `uvm_field_int(PADDR  , UVM_ALL_ON)
        `uvm_field_int(PWDATA , UVM_ALL_ON)
        `uvm_field_int(PRDATA , UVM_ALL_ON)
        `uvm_field_int(PREADY, UVM_ALL_ON)
        `uvm_field_int(PSLVERR, UVM_ALL_ON)
    `uvm_object_utils_end
endclass : APB_transaction

// override transaction
class APB_transaction_override extends APB_transaction;
    `uvm_object_utils(APB_transaction_override)

    function new(string name = "apb_transaction_override");
        super.new(name); 
        `uvm_info("OVERRIDE", "Override transaction test", UVM_LOW) 
    endfunction 
endclass : APB_transaction_override

class APB_test_config extends uvm_object;
    `uvm_object_utils(APB_test_config)
    
    virtual APB_if #(address_bus_width, data_bus_width) config_test_v_if;
    int TURN_ON_COV;
    
    function new(string name = "apb_test_config");
        super.new(name);
    endfunction
endclass

class APB_env_config extends uvm_object;
    `uvm_object_utils(APB_env_config)
    
    virtual APB_if #(address_bus_width, data_bus_width) config_env_v_if;
    int TURN_ON_COV;
    
    function new(string name = "apb_env_config");
        super.new(name);
    endfunction
endclass

class APB_driver_config extends uvm_object;
    `uvm_object_utils(APB_driver_config)
    
    virtual APB_if #(address_bus_width, data_bus_width) config_driver_v_if;
    
    function new(string name = "apb_driver_config");
        super.new(name);
    endfunction
endclass

class APB_cov_monitor_config extends uvm_object;
    `uvm_object_utils(APB_cov_monitor_config)
    
    virtual APB_if #(address_bus_width, data_bus_width) config_cov_monitor_v_if;
    int TURN_ON_COV;
    
    function new(string name = "apb_cov_monitor_config");
        super.new(name);
    endfunction
endclass

// sequence
class APB_base_sequence extends uvm_sequence #(APB_transaction);
	`uvm_object_utils(APB_base_sequence)
	
	APB_transaction tr;

    function new(string name = "APB_seq_1");
        super.new(name);
    endfunction
endclass : APB_base_sequence

// write test
class APB_write_sequence extends APB_base_sequence; 
    `uvm_object_utils(APB_write_sequence)
    
    function new(string name = "APB_write_seq");
        super.new(name);
    endfunction
	
    task body();	
        for (int i = 0; i < 32; i++) begin
            tr = APB_transaction::type_id::create("write_tr");
			start_item(tr);
	        
            if (i == 0) begin
                if (!tr.randomize() with {tr.PRESETn == 1'b0;
			    						  tr.PSEL == 1'b0;
			    						 })
                    `uvm_error("WRITE_SEQUENCE", "Randomization failure for transaction")				
            end
            else begin 
			    if (!tr.randomize() with {tr.PADDR[address_bus_width - 1 : address_bus_width - 4] == (i % 16);
			    						 tr.PWDATA inside {[32'hFFFF0000 : 32'hFFFFFFFF]};
			    						 tr.PRESETn == 1'b1;
			    						 tr.PWRITE == 1'b1;
			    						 tr.PSEL == 1'b1;
                                         tr.PENABLE == 1'b1;
			    						 })
                    `uvm_error("WRITE_SEQUENCE", "Randomization failure for transaction")				
            end
			
            finish_item(tr);
		end
    endtask
endclass : APB_write_sequence

// read test
class APB_read_sequence extends APB_base_sequence;
    `uvm_object_utils(APB_read_sequence)
    
    function new(string name = "APB_read_seq");
        super.new(name);
    endfunction
	
    task body();	
		for (int i = 0; i < 32; i++) begin
			tr = APB_transaction::type_id::create("read_tr");
			start_item(tr);
		    if (i == 0) begin
                if (!tr.randomize() with {tr.PRESETn == 1'b0;
			    						  tr.PSEL == 1'b0;
			    						 })
                    `uvm_error("WRITE_SEQUENCE", "Randomization failure for transaction")				
            end
            else begin	
			    if (!tr.randomize() with {tr.PADDR[address_bus_width - 1 : address_bus_width - 4] == (i % 16);
			    						  tr.PRESETn == 1'b1;
			    						  tr.PWRITE == 1'b0;
			    						  tr.PSEL == 1'b1;
                                          tr.PENABLE == 1'b1; 
			    						})
                    `uvm_error("READ_SEQUENCE", "Randomization failure for transaction")				
            end 

			finish_item(tr);
		end
    endtask
endclass : APB_read_sequence

// sequencer
class APB_sequencer extends uvm_sequencer #(APB_transaction);
    `uvm_component_utils(APB_sequencer)

    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction 
endclass : APB_sequencer

// driver
class APB_driver extends uvm_driver #(APB_transaction); 
    `uvm_component_utils(APB_driver)
    
    APB_env_config apb_env_config_h;
    APB_driver_config apb_driver_config_h;
    APB_transaction tr; 
    
    virtual APB_if #(.ADDR_BUS_WIDTH(address_bus_width), .DATA_BUS_WIDTH(data_bus_width)).APB_master apb_v_if; 
     
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction    
    
    function void configuration();
        apb_driver_config_h = new();
        assert(uvm_config_db #(APB_env_config)::get(this, "", "APB_env_config", apb_env_config_h))
        else begin
            `uvm_fatal("CONFIGURATION", "Driver failed to get configuration") 
        end 
        
        apb_driver_config_h.config_driver_v_if = apb_env_config_h.config_env_v_if; 
        
        apb_v_if = apb_driver_config_h.config_driver_v_if; 
    endfunction

    function void build_phase(uvm_phase phase);
        configuration(); 
    endfunction
    
    virtual task apb_test(virtual APB_if #(address_bus_width, data_bus_width).APB_master apb_master_modport);
        forever begin
          	@(apb_master_modport.cb_APB_master)
            seq_item_port.get_next_item(tr);
            if (tr.PRESETn == 1) begin 
                apb_master_modport.cb_APB_master.PRESETn <= tr.PRESETn;
                apb_master_modport.cb_APB_master.PSEL    <= tr.PSEL;
                apb_master_modport.cb_APB_master.PWRITE  <= tr.PWRITE;
                apb_master_modport.cb_APB_master.PADDR   <= tr.PADDR;
                apb_master_modport.cb_APB_master.PWDATA  <= tr.PWDATA; 
                apb_master_modport.cb_APB_master.PRDATA  <= tr.PRDATA; 
                @(apb_master_modport.cb_APB_master)
                apb_master_modport.cb_APB_master.PENABLE <= tr.PENABLE;
                repeat($urandom_range(2, 5)) @(apb_master_modport.cb_APB_master);
                apb_master_modport.cb_APB_master.PREADY  <= tr.PREADY;
                apb_master_modport.cb_APB_master.PSLVERR <= tr.PSLVERR;
                @(apb_master_modport.cb_APB_master)
                apb_master_modport.cb_APB_master.PSEL    <= 1'b0; 
                apb_master_modport.cb_APB_master.PWRITE  <= 1'b0; 
                apb_master_modport.cb_APB_master.PENABLE <= 1'b0; 
                apb_master_modport.cb_APB_master.PREADY  <= 1'b0; 
                apb_master_modport.cb_APB_master.PSLVERR <= 1'b0; 
            end
            else begin 
                apb_master_modport.cb_APB_master.PRESETn <= tr.PRESETn;
                apb_master_modport.cb_APB_master.PSEL    <= tr.PSEL;
                apb_master_modport.cb_APB_master.PWRITE  <= 1'b0; 
                apb_master_modport.cb_APB_master.PENABLE <= 1'b0; 
                apb_master_modport.cb_APB_master.PREADY  <= 1'b0; 
                apb_master_modport.cb_APB_master.PSLVERR <= 1'b0; 
            end
            seq_item_port.item_done();
        end 
    endtask : apb_test 

    task run_phase(uvm_phase phase);
        apb_test(apb_v_if);
    endtask
endclass : APB_driver

// override driver 
//class APB_driver_override extends APB_driver; 
//    `uvm_component_utils(APB_driver_override)
//
//    function new(string name, uvm_component parent);
//        super.new(name, parent);
//    endfunction
//
//    virtual task apb_test(virtual APB_if #(.ADDR_BUS_WIDTH(address_bus_width), .DATA_BUS_WIDTH(data_bus_width)).APB_master apb_master_modport);
//        forever begin
//                seq_item_port.get_next_item(tr);  
//        `uvm_info("OVERRIDE", "Override driver test", UVM_LOW) 
//                seq_item_port.item_done();
//        end
//    endtask
//    
//    task run_phase(uvm_phase phase);
//        apb_test(apb_v_if);
//    endtask
//endclass
  
class APB_cov_monitor extends uvm_monitor;
    `uvm_component_utils(APB_cov_monitor)
    
    APB_env_config apb_env_config_h; 
    APB_cov_monitor_config apb_cov_monitor_config_h; 
    
    virtual APB_if #(address_bus_width, data_bus_width).APB_monitor apb_v_if;
    
    function void configuration();
        apb_cov_monitor_config_h = new();
        assert(uvm_config_db #(APB_env_config)::get(this, "", "APB_env_config", apb_env_config_h))
        else begin
            `uvm_fatal("CONFIGURATION", "Monitor failed to get configuration") 
        end 
        
        apb_cov_monitor_config_h.config_cov_monitor_v_if = apb_env_config_h.config_env_v_if; 
        apb_cov_monitor_config_h.TURN_ON_COV = apb_env_config_h.TURN_ON_COV;  
        
        apb_v_if = apb_cov_monitor_config_h.config_cov_monitor_v_if; 
    endfunction

    covergroup apb_protocol_cg;
        option.per_instance = 1;

        RW: coverpoint apb_v_if.PWRITE {
            bins read  = {0};
            bins write = {1};
        }

        ERR: coverpoint apb_v_if.PSLVERR {
             bins err = {0};
             bins ok  = {1};
        }

        ADDR: coverpoint apb_v_if.PADDR[address_bus_width - 1 : address_bus_width - 4] {
              bins sel_0  = {0}; 
              bins sel_1  = {1}; 
              bins sel_2  = {2}; 
              bins sel_3  = {3}; 
              bins sel_4  = {4}; 
              bins sel_5  = {5}; 
              bins sel_6  = {6}; 
              bins sel_7  = {7}; 
              bins sel_8  = {8}; 
              bins sel_9  = {9}; 
              bins sel_10 = {10}; 
              bins sel_11 = {11}; 
              bins sel_12 = {12}; 
              bins sel_13 = {13}; 
              bins sel_14 = {14}; 
              bins sel_15 = {15}; 
        }
        
        cross RW, ERR, ADDR; 
    endgroup : apb_protocol_cg
 
    function new(string name, uvm_component parent);
        super.new(name, parent);
        apb_protocol_cg = new();
    endfunction 

    function void build_phase(uvm_phase phase);
        configuration(); 
    endfunction
    
    task run_phase(uvm_phase phase);
        if (apb_cov_monitor_config_h.TURN_ON_COV) begin 
            forever begin
                wait(apb_v_if.PREADY && apb_v_if.PENABLE);
                apb_protocol_cg.sample();
                wait(!apb_v_if.PREADY);
            end
        end
    endtask
endclass

class APB_environment extends uvm_env; 
    `uvm_component_utils(APB_environment)
   
    APB_test_config apb_test_config_h; 
    APB_env_config apb_env_config_h; 
    APB_driver apb_driver_h;    
    APB_cov_monitor apb_cov_monitor_h;    
    APB_sequencer apb_sequencer_h;    
 
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction 

    function void configuration();
        apb_env_config_h = new(); 
        
        assert(uvm_config_db #(APB_test_config)::get(this, "", "APB_test_config", apb_test_config_h))
        else begin
            `uvm_fatal("INTERFACE", "Environment configuration failed to get interface") 
        end 
        
        apb_env_config_h.config_env_v_if = apb_test_config_h.config_test_v_if;
        apb_env_config_h.TURN_ON_COV = apb_test_config_h.TURN_ON_COV;
        uvm_config_db #(APB_env_config)::set(this, "*", "APB_env_config", apb_env_config_h);
    endfunction 
    
    function void build_phase(uvm_phase phase);
        apb_driver_h      = APB_driver::type_id::create("apb_driver_h", this); 
        apb_cov_monitor_h = APB_cov_monitor::type_id::create("apb_cov_monitor_h", this); 
        apb_sequencer_h   = APB_sequencer::type_id::create("apb_sequencer_h", this);    
        configuration(); 
    endfunction

    function void connect_phase(uvm_phase phase);
        apb_driver_h.seq_item_port.connect(apb_sequencer_h.seq_item_export);
    endfunction
endclass : APB_environment 

class APB_test extends uvm_test; 
    `uvm_component_utils(APB_test)
    
    APB_test_config apb_test_config_h; 
    APB_environment apb_environment_h;
    APB_write_sequence apb_write_sequence_h;
    APB_read_sequence apb_read_sequence_h;
 
    function new(string name = "apb_test", uvm_component parent);
        super.new(name, parent);
    endfunction    
    
    function void configuration();
        apb_test_config_h = new(); 

        assert(uvm_config_db #(virtual APB_if #(address_bus_width, data_bus_width))::get(this, "", "apb_if", apb_test_config_h.config_test_v_if))
        else begin
            `uvm_fatal("INTERFACE", "Test configuration failed to get interface") 
        end 
        
        apb_test_config_h.TURN_ON_COV = 0;
        uvm_config_db #(APB_test_config)::set(this, "apb_environment_h", "APB_test_config", apb_test_config_h);
    endfunction

    function void build_phase(uvm_phase phase);
        apb_environment_h    = APB_environment::type_id::create("apb_environment_h", this);
        apb_write_sequence_h = APB_write_sequence::type_id::create("apb_write_sequence_h");  
        apb_read_sequence_h  = APB_read_sequence::type_id::create("apb_read_sequence_h");  
        configuration();
        //APB_driver::type_id::set_type_override(APB_driver_override::get_type()); 
    endfunction    
    
    task run_phase(uvm_phase phase);
        phase.raise_objection(this);
        apb_write_sequence_h.start(apb_environment_h.apb_sequencer_h); 
        apb_read_sequence_h.start(apb_environment_h.apb_sequencer_h); 
        phase.drop_objection(this);
    endtask 
endclass : APB_test 

module top;
    logic PCLK;

    APB_if #(address_bus_width, data_bus_width) dut_if(PCLK);
    
    APB_if_monitor apb_if_monitor(dut_if.APB_monitor);

    initial begin
        uvm_config_db #(virtual APB_if #(address_bus_width, data_bus_width))::set(null, "uvm_test_top", "apb_if", dut_if); 
        run_test("APB_test");
    end
    
    always 
        #5 PCLK = !PCLK;
    
    initial begin
        PCLK = 1'b1;
    end
endmodule : top

