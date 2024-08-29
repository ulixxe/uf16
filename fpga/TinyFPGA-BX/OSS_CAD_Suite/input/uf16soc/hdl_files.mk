# HDL files
HDL_FILES = \
phy_tx.v \
phy_rx.v \
sie.v \
ctrl_endp.v \
in_fifo.v \
out_fifo.v \
bulk_endp.v \
usb_cdc.v \
prescaler.v \
fifo_if.v \
timer.v \
gpio.v \
irqck.v \
vic.v \
SB_RAM256x16.v \
soc_rom.v \
soc_ram.v \
uf16.v \
app.v \
uf16soc.v \

# Testbench HDL files
TB_HDL_FILES = \
SB_PLL40_CORE.v \
usb_monitor.v \
tb_uf16soc.v \

# list of HDL files directories separated by ":"
VPATH = ../../../common/hdl: \
        ../../../common/hdl/ice40_lib: \
        ../../../common/hdl/usb_lib: \
        ../../../common/hdl/usb_cdc_lib: \
        ../../../common/hdl/ip_rtl_lib: \
        ../../../common/hdl/mem_ice40_lib: \
        ../../../uf16/hdl: \
        ../hdl/uf16soc: \

SIM_VPATH = ../../../common/hdl/mem_beh_lib: \
