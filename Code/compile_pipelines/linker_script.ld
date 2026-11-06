MEMORY
{
  TEXT (rx) : ORIGIN = 0x00000000, LENGTH = 0x0000FFFF
  DATA (rw) : ORIGIN = 0x00010000, LENGTH = 0x0000FFFF
}

SECTIONS
{
  .text : 
  {
    *(.text)
    . = ALIGN(4);
    PROVIDE(__global_pointer$ = . + 0x10200); 
  } > TEXT

  .data : 
  {
    *(.data)
    . = ALIGN(4); 
  } > DATA

  .bss :
  {
    PROVIDE(__bss_start = . + 0x400);
    *(.bss)
    *(COMMON)
    . = ALIGN(4); 
    PROVIDE(_end = .);
  } > DATA
}
