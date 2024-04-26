#include <cstddef>
    #include "verilated.h"
    #if VM_TRACE
        #ifndef DUMP_LEVEL
            #define DUMP_LEVEL 0
        #endif
        #ifdef DUMP_FST
            #include <verilated_fst_c.h>
            #define DUMP_TYPE VerilatedFstC
            // #define DUMP_FILE "dump.fst"
        #else
            #include <verilated_vcd_c.h>
            #define DUMP_TYPE VerilatedVcdC
            // #define DUMP_FILE "dump.vcd"
        #endif
    #endif
    #include "Vcounter.h"
    
// pyverilator defined values
// first declare variables as extern
extern const char* _pyverilator_module_name;
extern const uint32_t _pyverilator_num_inputs;
extern const char* _pyverilator_inputs[];
extern const uint32_t _pyverilator_input_widths[];
extern const uint32_t _pyverilator_num_outputs;
extern const char* _pyverilator_outputs[];
extern const uint32_t _pyverilator_output_widths[];
extern const uint32_t _pyverilator_num_internal_signals;
extern const char* _pyverilator_internal_signals[];
extern const uint32_t _pyverilator_internal_signal_widths[];
extern const uint32_t _pyverilator_num_rules;
extern const char* _pyverilator_rules[];
extern const char* _pyverilator_json_data;
// now initialize the variables
const char* _pyverilator_module_name = "counter";
const uint32_t _pyverilator_num_inputs = 3;
const char* _pyverilator_inputs[] = {"clk","rst","en"};
const uint32_t _pyverilator_input_widths[] = {1,1,1};

const uint32_t _pyverilator_num_outputs = 1;
const char* _pyverilator_outputs[] = {"out"};
const uint32_t _pyverilator_output_widths[] = {8};

const uint32_t _pyverilator_num_internal_signals = 0;
const char* _pyverilator_internal_signals[] = {};
const uint32_t _pyverilator_internal_signal_widths[] = {};

const char* _pyverilator_json_data = "null";

// this is required by verilator for verilog designs using $time
// main_time is incremented in eval
double main_time = 0;

// What to call when $finish is called
typedef void (*vl_finish_callback)(const char* filename, int line, const char* hier);
vl_finish_callback vl_user_finish = NULL;
    
double sc_time_stamp() {
return main_time;
}
void vl_finish (const char* filename, int linenum, const char* hier) VL_MT_UNSAFE {
    if (vl_user_finish) {
       (*vl_user_finish)(filename, linenum, hier);
    } else {
        // Default implementation
        VL_PRINTF("- %s:%d: Verilog $finish\n", filename, linenum);  // Not VL_PRINTF_MT, already on main thread
        if (Verilated::gotFinish()) {
            VL_PRINTF("- %s:%d: Second verilog $finish, exiting\n", filename, linenum);  // Not VL_PRINTF_MT, already on main thread
            Verilated::runFlushCallbacks();
            exit(0);
        }
        Verilated::gotFinish(true);
    }
}
// function definitions
// helper functions for basic verilator tasks
extern "C" { //Open an extern C closed in the footer
Vcounter* construct() {
    Verilated::traceEverOn(true);
    Vcounter* top = new Vcounter();
    return top;
}
int eval(Vcounter* top) {
    top->eval();
    main_time++;
    return 0;
}
int destruct(Vcounter* top) {
    if (top != nullptr) {
        delete top;
        top = nullptr;
    }
    return 0;
}
DUMP_TYPE* start_vcd_trace(Vcounter* top, const char* filename) {
    DUMP_TYPE* tfp = new DUMP_TYPE;
    top->trace(tfp, DUMP_LEVEL);
    tfp->open(filename);
    return tfp;
}
int add_to_vcd_trace(DUMP_TYPE* tfp, int time) {
    tfp->dump(time);
    return 0;
}
int flush_vcd_trace(DUMP_TYPE* tfp) {
    tfp->flush();
    return 0;
}
int stop_vcd_trace(DUMP_TYPE* tfp) {
    tfp->close();
    return 0;
}
bool get_finished() {
    return Verilated::gotFinish();
}
void set_finished(bool b) {
    Verilated::gotFinish(b);
}
void set_vl_finish_callback(vl_finish_callback callback) {
    vl_user_finish = callback;
}
void set_command_args(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);
}

uint32_t get_out(Vcounter* top){return top->out;}
uint32_t get_clk(Vcounter* top){return top->clk;}
uint32_t get_rst(Vcounter* top){return top->rst;}
uint32_t get_en(Vcounter* top){return top->en;}
int set_clk(Vcounter* top, uint32_t new_value){ top->clk = new_value; return 0;}
int set_rst(Vcounter* top, uint32_t new_value){ top->rst = new_value; return 0;}
int set_en(Vcounter* top, uint32_t new_value){ top->en = new_value; return 0;}
}