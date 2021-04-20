#!/bin/bash
./utils/bin/firrtl -td regress_template -i regress/ALU.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/ALU_area.txt &
./utils/bin/firrtl -td regress_template -i regress/FPU.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/FPU_area.txt &
./utils/bin/firrtl -td regress_template -i regress/HwachaSequencer.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/HwachaSequencer_area.txt &
./utils/bin/firrtl -td regress_template -i regress/ICache.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/ICache_area.txt &
./utils/bin/firrtl -td regress_template -i regress/Ops.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/Ops_area.txt &
./utils/bin/firrtl -td regress_template -i regress/Rob.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/Rob_area.txt &
./utils/bin/firrtl -td regress_template -i regress/RocketCore.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/RocketCore_area.txt &
./utils/bin/firrtl -td regress_template -i regress/ALU.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/ALU_time.txt &
./utils/bin/firrtl -td regress_template -i regress/FPU.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/FPU_time.txt &
./utils/bin/firrtl -td regress_template -i regress/HwachaSequencer.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/HwachaSequencer_time.txt &
./utils/bin/firrtl -td regress_template -i regress/ICache.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/ICache_time.txt &
./utils/bin/firrtl -td regress_template -i regress/Ops.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/Ops_time.txt &
./utils/bin/firrtl -td regress_template -i regress/Rob.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/Rob_time.txt &
./utils/bin/firrtl -td regress_template -i regress/RocketCore.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/RocketCore_time.txt &