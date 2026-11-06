circuit Seg7VerifyTop :
  module HexToSeg7 :
    input clock : Clock
    input reset : Reset
    output io : { flip hexDigit : UInt<4>, seg7 : UInt<8>}

    wire io_seg7_seg7 : UInt<8> @[Seg7.scala 63:20]
    node _io_seg7_seg7_T = eq(UInt<1>("h1"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_1 = mux(_io_seg7_seg7_T, UInt<8>("h9f"), UInt<2>("h3")) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_2 = eq(UInt<2>("h2"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_3 = mux(_io_seg7_seg7_T_2, UInt<6>("h25"), _io_seg7_seg7_T_1) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_4 = eq(UInt<2>("h3"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_5 = mux(_io_seg7_seg7_T_4, UInt<4>("hd"), _io_seg7_seg7_T_3) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_6 = eq(UInt<3>("h4"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_7 = mux(_io_seg7_seg7_T_6, UInt<8>("h9b"), _io_seg7_seg7_T_5) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_8 = eq(UInt<3>("h5"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_9 = mux(_io_seg7_seg7_T_8, UInt<7>("h49"), _io_seg7_seg7_T_7) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_10 = eq(UInt<3>("h6"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_11 = mux(_io_seg7_seg7_T_10, UInt<7>("h41"), _io_seg7_seg7_T_9) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_12 = eq(UInt<3>("h7"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_13 = mux(_io_seg7_seg7_T_12, UInt<5>("h1f"), _io_seg7_seg7_T_11) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_14 = eq(UInt<4>("h8"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_15 = mux(_io_seg7_seg7_T_14, UInt<1>("h1"), _io_seg7_seg7_T_13) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_16 = eq(UInt<4>("h9"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_17 = mux(_io_seg7_seg7_T_16, UInt<4>("h9"), _io_seg7_seg7_T_15) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_18 = eq(UInt<4>("ha"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_19 = mux(_io_seg7_seg7_T_18, UInt<5>("h11"), _io_seg7_seg7_T_17) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_20 = eq(UInt<4>("hb"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_21 = mux(_io_seg7_seg7_T_20, UInt<8>("hc1"), _io_seg7_seg7_T_19) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_22 = eq(UInt<4>("hc"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_23 = mux(_io_seg7_seg7_T_22, UInt<7>("h61"), _io_seg7_seg7_T_21) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_24 = eq(UInt<4>("hd"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_25 = mux(_io_seg7_seg7_T_24, UInt<8>("h85"), _io_seg7_seg7_T_23) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_26 = eq(UInt<4>("he"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_27 = mux(_io_seg7_seg7_T_26, UInt<7>("h61"), _io_seg7_seg7_T_25) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_28 = eq(UInt<4>("hf"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_29 = mux(_io_seg7_seg7_T_28, UInt<7>("h71"), _io_seg7_seg7_T_27) @[Mux.scala 81:58]
    io_seg7_seg7 <= _io_seg7_seg7_T_29 @[Seg7.scala 64:10]
    io.seg7 <= io_seg7_seg7 @[Seg7.scala 92:11]

  module Seg7 :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { flip seg7 : UInt<32>}, board : { seg7 : UInt<8>, an : UInt<8>}}

    reg clock_counter : UInt<32>, clock with :
      reset => (reset, UInt<32>("h186a0")) @[Seg7.scala 25:30]
    node _clock_counter_T = sub(clock_counter, UInt<1>("h1")) @[Seg7.scala 27:34]
    node _clock_counter_T_1 = tail(_clock_counter_T, 1) @[Seg7.scala 27:34]
    clock_counter <= _clock_counter_T_1 @[Seg7.scala 27:17]
    reg counter : UInt<3>, clock with :
      reset => (reset, UInt<3>("h0")) @[Seg7.scala 29:24]
    node _T = eq(clock_counter, UInt<1>("h0")) @[Seg7.scala 31:22]
    when _T : @[Seg7.scala 31:31]
      clock_counter <= UInt<17>("h186a0") @[Seg7.scala 32:19]
      node _counter_T = add(counter, UInt<1>("h1")) @[Seg7.scala 33:24]
      node _counter_T_1 = tail(_counter_T, 1) @[Seg7.scala 33:24]
      counter <= _counter_T_1 @[Seg7.scala 33:13]
    else :
      skip
    node _digits_T = bits(io.mmio.seg7, 3, 0) @[Seg7.scala 39:57]
    node _digits_T_1 = bits(io.mmio.seg7, 7, 4) @[Seg7.scala 39:57]
    node _digits_T_2 = bits(io.mmio.seg7, 11, 8) @[Seg7.scala 39:57]
    node _digits_T_3 = bits(io.mmio.seg7, 15, 12) @[Seg7.scala 39:57]
    node _digits_T_4 = bits(io.mmio.seg7, 19, 16) @[Seg7.scala 39:57]
    node _digits_T_5 = bits(io.mmio.seg7, 23, 20) @[Seg7.scala 39:57]
    node _digits_T_6 = bits(io.mmio.seg7, 27, 24) @[Seg7.scala 39:57]
    node _digits_T_7 = bits(io.mmio.seg7, 31, 28) @[Seg7.scala 39:57]
    wire digits : UInt<4>[8] @[Seg7.scala 39:23]
    digits[0] <= _digits_T @[Seg7.scala 39:23]
    digits[1] <= _digits_T_1 @[Seg7.scala 39:23]
    digits[2] <= _digits_T_2 @[Seg7.scala 39:23]
    digits[3] <= _digits_T_3 @[Seg7.scala 39:23]
    digits[4] <= _digits_T_4 @[Seg7.scala 39:23]
    digits[5] <= _digits_T_5 @[Seg7.scala 39:23]
    digits[6] <= _digits_T_6 @[Seg7.scala 39:23]
    digits[7] <= _digits_T_7 @[Seg7.scala 39:23]
    inst seg7Module of HexToSeg7 @[Seg7.scala 45:26]
    seg7Module.clock <= clock
    seg7Module.reset <= reset
    seg7Module.io.hexDigit <= digits[counter] @[Seg7.scala 46:26]
    io.board.seg7 <= seg7Module.io.seg7 @[Seg7.scala 47:17]
    node _io_board_an_T = eq(UInt<1>("h1"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_1 = mux(_io_board_an_T, UInt<8>("hfd"), UInt<8>("hfe")) @[Mux.scala 81:58]
    node _io_board_an_T_2 = eq(UInt<2>("h2"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_3 = mux(_io_board_an_T_2, UInt<8>("hfb"), _io_board_an_T_1) @[Mux.scala 81:58]
    node _io_board_an_T_4 = eq(UInt<2>("h3"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_5 = mux(_io_board_an_T_4, UInt<8>("hf7"), _io_board_an_T_3) @[Mux.scala 81:58]
    node _io_board_an_T_6 = eq(UInt<3>("h4"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_7 = mux(_io_board_an_T_6, UInt<8>("hef"), _io_board_an_T_5) @[Mux.scala 81:58]
    node _io_board_an_T_8 = eq(UInt<3>("h5"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_9 = mux(_io_board_an_T_8, UInt<8>("hdf"), _io_board_an_T_7) @[Mux.scala 81:58]
    node _io_board_an_T_10 = eq(UInt<3>("h6"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_11 = mux(_io_board_an_T_10, UInt<8>("hbf"), _io_board_an_T_9) @[Mux.scala 81:58]
    node _io_board_an_T_12 = eq(UInt<3>("h7"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_13 = mux(_io_board_an_T_12, UInt<7>("h7f"), _io_board_an_T_11) @[Mux.scala 81:58]
    io.board.an <= _io_board_an_T_13 @[Seg7.scala 49:15]

  module Seg7VerifyTop :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip seg7_in : UInt<16>, seg7_out : { seg7 : UInt<8>, an : UInt<8>}}

    inst seg7 of Seg7 @[Seg7VerifyTop.scala 12:20]
    seg7.clock <= clock
    seg7.reset <= reset
    node _seg7_io_mmio_seg7_T = cat(UInt<16>("h0"), io.seg7_in) @[Cat.scala 33:92]
    seg7.io.mmio.seg7 <= _seg7_io_mmio_seg7_T @[Seg7VerifyTop.scala 13:21]
    io.seg7_out <= seg7.io.board @[Seg7VerifyTop.scala 14:15]

