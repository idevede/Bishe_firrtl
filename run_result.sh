#!/bin/bash
./utils/bin/firrtl -td regress_allinone -i regress/ALU.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/ALU_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/FPU.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/FPU_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/HwachaSequencer.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/HwachaSequencer_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/ICache.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/ICache_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/Ops.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/Ops_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/Rob.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/Rob_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/RocketCore.fir --custom-transforms tutorial.lesson3.ReportArea >>./regress/result/RocketCore_area.out &
./utils/bin/firrtl -td regress_allinone -i regress/ALU.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/ALU_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/FPU.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/FPU_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/HwachaSequencer.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/HwachaSequencer_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/ICache.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/ICache_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/Ops.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/Ops_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/Rob.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/Rob_time.out &
./utils/bin/firrtl -td regress_allinone -i regress/RocketCore.fir --custom-transforms tutorial.lesson3.ReportTimingFull >>./regress/result/RocketCore_time.out &