out:	transpose.cu
	nvcc -arch=sm_60 -o $@ $<
profile:
	nvprof --analysis-metrics -f -o output.nvvp ./out
clean:
	rm -rf out output.nvvp
