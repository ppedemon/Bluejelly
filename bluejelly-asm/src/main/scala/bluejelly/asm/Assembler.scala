/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.io.IOException
import java.io.PrintWriter
import java.io.StringWriter
import java.util.Properties

import org.objectweb.asm.ClassWriter.COMPUTE_MAXS
import org.objectweb.asm.Opcodes.ACC_FINAL
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.objectweb.asm.Opcodes.RETURN
import org.objectweb.asm.Opcodes.V1_6
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.FieldVisitor

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import scala.collection.mutable.MutableList

/*
 * Assembler configuration
 */
class AsmConfig {
  var version = false
  var prettyPrint = false
  var debugInfo = false
  var outDir = "."
  var files:MutableList[String] = MutableList()
}

/*
 * Name helper, qualifies fun ids if necessary to the current module's name. 
 */
private class IdQualifier(moduleName:String) {
  private def isQualified(funId:String) = funId.indexOf('.') != -1
  def qualifiy(funId:String) = moduleName + '.' + funId
  def ensureQual(funId:String) = if (isQualified(funId)) funId else qualifiy(funId)
}

/** 
 * Generate Java code from an assembler file.
 * @author ppedemon
 */
class Assembler(cfg:AsmConfig, m:Module) {

  import org.objectweb.asm.ClassWriter._
  import org.objectweb.asm.Opcodes._
  import ByteCodeHelper._
  
  private val qual:IdQualifier = new IdQualifier(m.name)
  
  /** 
   * Assemble a whole module: functions + dictionaries.
   * @param m [[bluejelly.asm.Module]] to assemble
   */
  def assemble {    
    // Create class for module to compile
    val name = m.name.replace('.','/')
    val w:ClassWriter = new ClassWriter(COMPUTE_FRAMES)
  	w.visit(V1_6, ACC_PUBLIC + ACC_FINAL, name, null, 
  	  "java/lang/Object", Array("bluejelly/runtime/Module"));
        
    // Generate default constructor, initialization of 
    // the fields representing dictionaries goes here
    val v = w.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    v.visitCode
    v.visitIntInsn(ALOAD, 0)
    v.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
    v.visitInsn(RETURN)
    v.visitMaxs(0,0)
    v.visitEnd
    
    // Generate code for functions
    for (f <- m funcs) assemble(w, f)
    w.visitEnd

    // Save class
    val bytes:Array[Byte] = w.toByteArray
    save(name, bytes)
  }
    
  // Assemble a function
  private def assemble(w:ClassWriter, f:Function) {
    val desc = "(Lbluejelly/runtime/ExecutionContext;)V"
    val start = new Label
    val end = new Label
    val v = w.visitMethod(ACC_PUBLIC  + ACC_FINAL, f name, desc, null, null)
    annotate(v, f)
    v.visitCode    
    if (cfg.debugInfo) {
      v.visitLabel(start)
    }
    assemble(v, f.b)
    if (cfg.debugInfo) {
      v.visitLabel(end)
      v.visitLocalVariable("ctx", toDesc(ctx), null, start, end, 1)
    }
    v.visitMaxs(0,0)
    v.visitEnd
  }
  
  // Add @JellyCode annotation for some function
  private def annotate(v:MethodVisitor, f:Function) {
    val a = v.visitAnnotation("Lbluejelly/runtime/ann/JellyCode;", true)
    if (f.arity > 0) a.visit("arity", f.arity)
    if (f.matcher) a.visit("matcher", true)
    a.visitEnd()
  }
  
  // Assemble a block
  private def assemble(v:MethodVisitor, b:Block) {
    for (i <- b.is) assemble(v,i)
  }
  
  // Assemble instructions, real work done here
  private def assemble(v:MethodVisitor, i:Instr) = i match {
    case Enter  => mkFinal(scala.Function.const())(v)
    case Return => mkFinal(scala.Function.const())(v)
    case Catch  => invokeVoidCtxMethod("registerCatch")(v)
    case Raise  => mkFinal(invokeVoidCtxMethod("raise"))(v)
    
    case RetCon(tag,n) => mkFinal(invokeIntIntCtxMethod("retTyCon", tag, n))(v)
    case RetInt(n)     => mkFinal(invokeIntCtxMethod("retInt", n))(v)
    case RetStr(s)     => mkFinal(invokeStrCtxMethod("retStr", s))(v)
    case RetChr(c)     => mkFinal({ v =>
      v.visitIntInsn(ALOAD, 1)
      pushIntConst(v, c)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "retChar", "(C)V")
    })(v)
    case RetDbl(d)     => mkFinal({ v =>
      v.visitIntInsn(ALOAD, 1)
      v.visitLdcInsn(d)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "retDouble", "(D)V")
    })(v)
    
    case Jump(fun) => 
      mkFinal(invokeStrCtxMethod("jump", qual.ensureQual(fun)))(v) 
    case EvalVar(off,funId) => 
      mkFinal(invokeIntStrCtxMethod("evalVar", off, qual.ensureQual(funId)))(v)
    
    case StackCheck(n) => invokeIntCtxMethod("stackCheck", n)(v)
    case DumpStack(s) => if (cfg.debugInfo) {
      invokeStrCtxMethod("dumpStack", s)(v)
    }
    case PushVar(off)  => pushVar(v,off)
    case PushInt(n)    => pushInt(v,n)
    case PushChr(c)    => pushChar(v,c)
    case PushDbl(d)    => pushDbl(v,d)
    case PushStr(s)    => pushStr(v,s)
    case PushCode(fun) => pushCode(v, qual.ensureQual(fun))
    case PushCont(fun) => invokeStrCtxMethod("pushCont", qual.ensureQual(fun))(v)
    case Slide(off,n)  => invokeIntIntCtxMethod("slide", off,n)(v)
    
    case MkApp(n)        => invokeIntCtxMethod("mkApp", n)(v)
    case MkNapp(n)       => invokeIntCtxMethod("mkNApp", n)(v)
    case AllocApp        => invokeVoidCtxMethod("allocApp")(v)
    case AllocNapp       => invokeVoidCtxMethod("allocNApp")(v)
    case PackApp(off,n)  => invokeIntIntCtxMethod("packApp", off, n)(v)
    case PackNapp(off,n) => invokeIntIntCtxMethod("packNApp", off, n)(v)
    
    case MkTyCon(tag,n)   => invokeIntIntCtxMethod("mkTyCon", tag, n)(v)
    case AllocTyCon(tag)  => invokeIntCtxMethod("allocTyCon", tag)(v)
    case PackTyCon(off,n) => invokeIntIntCtxMethod("packTyCon", off, n)(v)
        
    case MatchCon(alts,mdef) => {
      v.visitIntInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getTag", "()I")
      compileSwitch(alts, mdef, "altTyConPrologue", "defTyConPrologue")(v)
    } 
    case MatchInt(alts,mdef) => {
      v.visitIntInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getInt", "()I")
      compileSwitch(alts, mdef, "altBasicPrologue", "defIntPrologue")(v)
    }
    case MatchChr(alts,mdef) => {
      val intAlts = alts map (a => new Alt[Int](a.v.toInt,a.b))
      v.visitIntInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getChar", "()C")
      compileSwitch(intAlts, mdef, "altBasicPrologue", "defCharPrologue")(v)      
    }
    case MatchDbl(alts,mdef) => {
      v.visitIntInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getDouble", "()D")
      compileDoubleSwitch(alts, mdef)(v)
    }
    case MatchStr(alts,mdef) => {
      v.visitIntInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getStr", "()Ljava/lang/String;")
      compileStrSwitch(alts, mdef)(v)      
    }
  }
  
  // Minimum density acceptable for table switch instructions. If 
  // density drops below this value, we will use a lookup switch.
  val minDensity= 0.4
  
  // Convenience type pairing an alternative to the label used to identify it
  type CaseOpt[T] = (Label,Alt[T])
  
  private def altBounds(alts:List[Alt[Int]]) = alts match {
    case Nil => (0,0)
    case _   => (alts.minBy(_.v).v,alts.maxBy(_.v).v)
  }
    
  
  private def denseEnough(alts:List[Alt[Int]], min:Int, max:Int):Boolean =
    alts.length.toDouble/(max - min + 1) >= minDensity
    
  /** 
   * Compile a list of alternatives into a switch. It's important to keep
   * in mind that every alternative returns, hence we don't need a join 
   * label where all the alternatives jump (in lay terms, we don't have to 
   * implement the `break' instruction). The alternatives might refer to 
   * constructor or int matching.
   * 
   * @param alts           list of case alternatives to compile
   * @param mdef           Some(b), b default block, or None
   * @param prologue       name of case alternative prologue method
   * @param defPrologue    name of default alternative prologue method
   */
  private def compileSwitch(
      alts:List[Alt[Int]], 
      mdef:Option[Block], 
      prologue:String,
      defPrologue:String)(v:MethodVisitor) {
    
    val (min,max) = altBounds(alts)    
    val deflt = new Label
    val opts = (alts :\ SortedMap[Int,CaseOpt[Int]]()) 
      {(a,m) => m + (a.v -> (new Label,a))}

    if (denseEnough(alts, min, max)) {
      val labels:Array[Label] = new Array(max - min + 1)
      val kd:PartialFunction[Int,Label] = {case _ => deflt} 
      for (i <- min to max) labels(i - min) = (opts andThen (_._1) orElse kd)(i)
      v.visitTableSwitchInsn(min, max, deflt, labels)
    } else {
      v.visitLookupSwitchInsn(deflt, opts.keys.toArray, 
          opts.values.map(_._1).toArray)
    }
    
    alts foreach {a => compileAlt(v, opts(a.v), prologue)}
    compileDeflt(v, deflt, mdef, defPrologue)
  }
  
  // Compile a single alternative
  private def compileAlt(v:MethodVisitor, caseOpt:CaseOpt[_], prologue:String) {
    v.visitLabel(caseOpt._1)
    invokeVoidCtxMethod(prologue)(v)
    assemble(v,caseOpt._2.b)
  }
  
  // Possibly compile a default block
  private def compileDeflt(
      v:MethodVisitor, 
      deflt:Label, 
      mdef:Option[Block], 
      defPrologue:String) {
    
    v.visitLabel(deflt)
    mdef map {b => invokeVoidCtxMethod(defPrologue)(v); assemble(v,b)} getOrElse 
      {mkFinal(invokeVoidCtxMethod("raisePatternMatchFail"))(v)}
  }
  
  /**
   * Compile a matchdbl instruction as a sequence of chained ifs. Like
   * the matchcon and matchint instructions, each alternative returns.
   * 
   * @param alts    case alternatives
   * @param deflt   Some(b), b default block, or None
   */
  private def compileDoubleSwitch(
      alts:List[Alt[Double]], 
      mdef:Option[Block])(v:MethodVisitor) {
    
    v.visitVarInsn(DSTORE, 2)
    val deflt = ((None:Option[Label]) /: alts) {(label,a) => 
      label foreach {v.visitLabel(_)}
      val next = new Label
      v.visitVarInsn(DLOAD, 2)
      v.visitLdcInsn(a.v)
      v.visitInsn(DCMPL)
      v.visitJumpInsn(IFNE, next)
      invokeVoidCtxMethod("altBasicPrologue")(v)
      assemble(v, a.b)
      Some(next)
    }
    deflt foreach {compileDeflt(v, _, mdef, "defDoublePrologue")}
  }

  /**
   * Compile a matchstr instruction as a combination of a case and 
   * ifs for each alternative. For example, for the following code:
   * 
   * <code>
   * switch(s) {
   * case "foo": <block #1>; break
   * case "bar": <block #2>
   * default   : <default block>
   * }
   * </code>
   * 
   * We get the following pseudo-code:
   * 
   * <code>
   *   int h = s.hashCode()
   *   switch(h) {
   *   case hash("foo"): if (s.equals("foo")) goto L1 else goto L3
   *   case hash("bar"): if (s.equals("bar")) goto L2 else goto L3
   *   }
   *   L1: <block #1>; goto Join
   *   L2: <block #2>; 
   *   L3: <default block>
   *   Join: return
   *   }
   * </code>
   * 
   * We call the table starting at L1 the <em>action table</em>.
   * 
   * <p> If the case has a single alternative, this imposes too much 
   * overhead. In that case, we simple do a plain string comparison. 
   * We compile the switch directly to a lookupswitch, thus assuming
   * that hash codes are going to be disperse enough (they should!). 
   * 
   * @param alts    case alternatives
   * @param deflt   Some(b), b default block, or None
   */
  private def compileStrSwitch(
      alts:List[Alt[String]], 
      mdef:Option[Block])(v:MethodVisitor) {
    
    val deflt = new Label
    
    // If there is just a single alt, just compare strings
    if (alts.length == 1) {
      v.visitLdcInsn(alts.head.v)
      v.visitMethodInsn(INVOKEVIRTUAL, 
          "java/lang/String", "equals", "(Ljava/lang/Object;)Z")
      v.visitJumpInsn(IFEQ, deflt)
      invokeVoidCtxMethod("altBasicPrologue")(v)
      assemble(v, alts.head.b)
    } 
    // Otherwise, try to be clever
    else {    
      val opts = (Map[String,CaseOpt[String]]() /: alts) 
        {(m,a) => m + (a.v -> (new Label,a))}   
      val groups = alts map (_.v) groupBy (_.hashCode) mapValues ((new Label,_))
      val sortedGroups = TreeMap(groups.toArray:_*)
      v.visitVarInsn(ASTORE, 2)
      v.visitVarInsn(ALOAD, 2)
      v.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "hashCode", "()I")
      v.visitLookupSwitchInsn(
          deflt, 
          sortedGroups.keys.toArray, 
          sortedGroups.values.map(_._1).toArray)
      compileGroups(v,deflt,sortedGroups,opts)
      compileOpts(v, opts)
    }
    
    compileDeflt(v, deflt, mdef, "defStrPrologue")
  }
    
  // Compile case alternatives each option jumps 
  // to a specific entry of the action table
  private def compileGroups(
      v:MethodVisitor,
      deflt:Label,
      gs:Map[Int,(Label,List[String])],
      opts:Map[String,CaseOpt[String]]) {
    
    gs foreach { case (_,(caseLabel,strs)) =>
      v.visitLabel(caseLabel)
	  val last = ((None:Option[Label]) /: strs) {(label,s) => 
	    label foreach {v.visitLabel(_)}
	    val next = new Label
	    v.visitVarInsn(ALOAD, 2)
	    v.visitLdcInsn(s)
	    v.visitMethodInsn(INVOKEVIRTUAL, 
	        "java/lang/String", "equals", "(Ljava/lang/Object;)Z")
	    v.visitJumpInsn(IFEQ, next)
	    v.visitJumpInsn(GOTO, opts(s)._1)
	    Some(next)
	  }
      last foreach {v.visitLabel(_)}
      v.visitJumpInsn(GOTO, deflt)
    }
  }
  
  // Compile action table for string matching
  private def compileOpts(v:MethodVisitor, opts:Map[String,CaseOpt[String]]) {
    opts.values.foreach { case (label,a) =>
      v.visitLabel(label)
      invokeVoidCtxMethod("altBasicPrologue")(v)
      assemble(v, a.b)
    }
  }
  
  //Save generated class
  private def save(name:String, bytes:Array[Byte]) {
    val fullName = cfg.outDir + '/' + name + ".class"
    val ix = fullName lastIndexOf '/'
    val path = fullName.take(ix)
    new File(path).mkdirs()
    val out = new FileOutputStream(fullName)
    out write bytes
    out close
  }
  
}

/**
 * Companion object providing the assembler's entry point.
 */
object Assembler {  
  
  import bluejelly.utils._
  
  private val appName = "bas"
  private val version = "The Bluejelly Assembler, v" + appVersion
  
  private val cfg = new AsmConfig  
  private val opts = List(
      ("-p", new UnitOpt(_ => cfg.prettyPrint = true, "-p Show parser results and leave")),
      ("-v", new UnitOpt(_ => cfg.version = true, "-v Show version information")),
      ("-g", new UnitOpt(_ => cfg.debugInfo = true, "-g Add debug info to compiled code")),
      ("-d", new StrOpt(cfg.outDir = _,"-d [dir]~Output directory for compiled code")))      
  private val arg = new Args(
      opts, 
      cfg.files += _, 
      "Usage: " + appName + " [options] files...")
  
  private def appVersion:String = {
    try {
      val p = new Properties()
      p.load(classOf[Assembler].getResourceAsStream("/bas-cfg.properties"))
      p.getProperty("bas.version")
    } catch {
      case e:IOException => "<?>"
    }
  }

  // Process a single file
  def assemble(cfg:AsmConfig, sourceName:String) {
    val r = new UnicodeFilter(new FileReader(sourceName))
    val p = Parser.parseAll(Parser.module, r)
    p match {
      case f@Parser.Failure(_,_) => println(f)
      case Parser.Success(m,_) => {
        if (cfg.prettyPrint) {
          val w = new PrintWriter(System.out)
          m.ppr(w)(0)
          w flush()
        } else {
          val v = new Validator(m)
          val (ok,errs) = v validate()
          if (ok) {
            val a = new Assembler(cfg,m)
            a assemble
          } else {
            v dumpErrs (errs, new PrintWriter(System.err))
            val e = if (errs.length == 1) "error" else "errors"
            System.err.println(
              "Found %d %s, compilation of `%s' aborted!\n" format (
                  errs.length,e,sourceName))
          }
        } 
      }
    }
  }
  
  def main(argv:Array[String]) {
    if (!arg.parse(argv)) return
    
    if (arg.helpInvoked) return
    if (cfg.version) {println(version); return}
    if (cfg.files.isEmpty) {
      println(appName + ": no input files")
      println("Use -h or --help for a list of possible options")
      return;
    }
    cfg.files foreach {assemble(cfg,_)}
  }

}
