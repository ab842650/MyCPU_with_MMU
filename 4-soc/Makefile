# SPDX-License-Identifier: MIT
# MyCPU is freely redistributable under the MIT License. See the file
# "LICENSE" for information on usage and redistribution of this file.

# Include common build utilities
include build.mk

test:
	cd .. && sbt "project soc" test

verilator:
	@if java -version >/dev/null 2>&1; then \
		cd .. && PATH=$$HOME/.local/bin:$$PATH sbt "project soc" "runMain board.verilator.VerilogGenerator"; \
	else \
		echo "⚠️  Java runtime not found; using existing generated Verilog in verilog/verilator"; \
		if [ ! -f verilog/verilator/Top.v ]; then \
			echo "❌ Top.v missing; install Java (set JAVA_HOME) to regenerate Verilog"; \
			exit 1; \
		fi; \
	fi
	cd verilog/verilator && verilator --exe --cc sim.cpp Top.v \
		-CFLAGS "$$(sdl2-config --cflags)" \
		-LDFLAGS "$$(sdl2-config --libs)" && \
		make -C obj_dir -f VTop.mk

demo: verilator
	@echo "🔄 Building nyancat demo binary..."
	@$(MAKE) -C csrc nyancat.asmbin >/dev/null
	@echo "🎮 Running nyancat demo with SDL2 display..."
	@echo "   - Press ESC or close window to exit"
	@echo "   - VGA output: 640×480 @ 72Hz"
	@echo ""
	cd verilog/verilator/obj_dir && ./VTop -i ../../../csrc/nyancat.asmbin
	@echo ""
	@echo "✅ Demo complete!"

indent:
	find . -name '*.scala' | xargs scalafmt
	clang-format -i verilog/verilator/*.cpp verilog/verilator/*.h
	clang-format -i csrc/*.[ch]

compliance: check-riscof
	@echo "Running RISCOF compliance tests for 4-soc (RV32I + Zicsr)..."
	@cd ../tests && RISCOF_WORK=riscof_work_4soc ./run-compliance.sh 4-soc
	@echo ""
	@echo "Copying results to results/ directory..."
	@$(RM) -r results
	@mkdir -p results
	@cp -r ../tests/riscof_work_4soc/* results/
	@echo "Cleaning up auto-generated RISCOF test files..."
	@$(RM) -f src/test/scala/riscv/compliance/ComplianceTest.scala
	@$(RM) -f src/main/resources/test.asmbin
	@echo "✅ Compliance tests complete. Results in results/"
	@echo "📊 View report: results/report.html"

clean:
	cd .. && sbt "project soc" clean
	$(RM) -r test_run_dir
	$(RM) -r verilog/verilator/obj_dir
	$(RM) verilog/verilator/*.v
	$(RM) verilog/verilator/*.fir
	$(RM) verilog/verilator/*.anno.json

distclean: clean
	$(RM) -r results

.PHONY: verilator test indent demo compliance clean distclean
