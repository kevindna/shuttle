package shuttle.common

import chisel3._
import chisel3.util._

// Zvt v0.4 (RISC-V matrix extension) encodings and the mtype CSR, for the
// Shuttle core (configuration instructions, mtype, checks) and the vector unit
// (shuttle.common so both can use it)

// mtype CSR (0xC23): tm [23:10], tk [7:5], mtwiden [1:0]
class ZvtMType extends Bundle {
  val tm      = UInt(14.W)
  val tk      = UInt(3.W)
  val mtwiden = UInt(2.W)
  def asCSR: UInt = Cat(tm, 0.U(2.W), tk, 0.U(3.W), mtwiden)
  // log2(TWIDEN); mtwiden = 0 (not configured) reads as TWIDEN = 1
  def twidenLog: UInt = Mux(mtwiden === 0.U, 0.U, mtwiden - 1.U)
}

object ZvtMType {
  val csrAddr = 0xC23
  def fromCSR(x: UInt): ZvtMType = {
    val m = Wire(new ZvtMType)
    m.tm      := x(23, 10)
    m.tk      := x(7, 5)
    m.mtwiden := x(1, 0)
    m
  }
}

object Zvt {
  // configuration instructions (OP-V, funct3 = 111)
  val MSETTN     = BitPat("b100010000000?????111?????1010111")
  val MSETTM     = BitPat("b100010000001?????111?????1010111")
  val MSETTK     = BitPat("b100010000010?????111?????1010111")
  val MSETMTYPEI = BitPat("b100?100??100000??111000001010111")
  val MSETMTYPE  = BitPat("b1000010??????????111000001010111")
  // matrix arithmetic (OP-VE): vtfmm[.alt].tvv (funct3 001), vtmmu / vtmms.tvv (funct3 000)
  val MATMUL     = BitPat("b1111001??????????00????0?1110111")
  // vtle / vtse: nf = EEW, mew = 1, mop = 00, vm = 1, width = 111, vd / vs3 = 0
  val TILE_LS    = BitPat("b???1001??????????111000000?00111")
  // moves, tile zero, context discard (OP-V, OPMVX)
  val VTMV_V_T   = BitPat("b010000111111?????110?????1010111")
  val VTMV_T_V   = BitPat("b0101111??????????110000001010111")
  val VTZERO     = BitPat("b01000011111000000110????01010111")
  val VTDISCARD  = BitPat("b01000011110000000110000001010111")
  // mvin_bcast as a Zvt-style instruction (not in the spec; an unused OPMVX
  // funct6): vtbcast.t.v mtd, vs2 writes the vs2 row to every row of tile mtd
  val VTBCAST    = BitPat("b0110001?????00000110????01010111")

  def isConfig(i: UInt): Bool = Seq(MSETTN, MSETTM, MSETTK, MSETMTYPEI, MSETMTYPE).map(_ === i).reduce(_ || _)
  // instructions that access the tile state (illegal with MS = Off, set MS Dirty)
  def accessesTile(i: UInt): Bool = Seq(MATMUL, TILE_LS, VTMV_V_T, VTMV_T_V, VTZERO, VTBCAST).map(_ === i).reduce(_ || _)

  // KMAX = 4 / 2 / 1 for SEW = 8 / 16 / >= 32: log2
  def kmaxLog(vsew: UInt): UInt = Mux(vsew === 0.U, 2.U, Mux(vsew === 1.U, 1.U, 0.U))
  // tile specifier t valid for the TEW (log2 bytes): t mod (16 / NTiles) == 0
  def tileOk(t: UInt, tewLog: UInt): Bool = MuxLookup(tewLog, false.B)(Seq(
    0.U -> true.B, 1.U -> !t(0), 2.U -> (t(1, 0) === 0.U), 3.U -> !t(0)))
  // matmul operand register vs: vs mod 8 < 8 / KMAX, and vs divisible by LMUL
  def operandOk(vs: UInt, vsew: UInt, vlmulSign: Bool, vlmulMag: UInt): Bool = {
    val modOk = (vs(2, 0) >> (3.U - kmaxLog(vsew)))(2, 0) === 0.U
    val lmulOk = vlmulSign || ((vs & ((1.U(8.W) << vlmulMag) - 1.U)(4, 0)) === 0.U)
    modOk && lmulOk
  }
}
