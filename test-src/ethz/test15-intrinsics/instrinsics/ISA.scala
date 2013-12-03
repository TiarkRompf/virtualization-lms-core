package ethz.test15.instrinsics

object ISA extends Enumeration {
  type InstructionSets = Value
  val None, MMX, SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AVX, AVX2 = Value
}
