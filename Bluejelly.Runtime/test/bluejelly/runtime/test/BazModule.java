/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.test;

import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.Module;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.Node;

/**
 * Another dummy module for runtime testing purposes.
 * 
 * @author ppedemon
 */
public class BazModule implements Module {

    @JellyCode
    public void dummyLetrec(ExecutionContext ctx) {
        ctx.stackCheck(5);
        
        int sp;
        Node[] s = ctx.s;
        
        // x
        ctx.allocNApp();
        // y
        ctx.allocApp();
        
        // x = swap y 1
        sp = ctx.sp;
        s[++sp] = Int.mkInt(1);
        s[sp+1] = s[sp-1]; ++sp;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule.swap");
        
        ctx.sp = sp;
        ctx.packNApp(1, 3);
        
        // y = swap 2 3
        sp = ctx.sp;
        s[++sp] = Int.mkInt(3);
        s[++sp] = Int.mkInt(2);
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule.swap");
        
        ctx.sp = sp;
        ctx.packApp(0, 3);
        
        // PushVar 1
        sp = ctx.sp;        
        s[sp+1] = s[sp-1]; ++sp;
        // PushInt 0
        s[++sp] = Int.mkInt(0);
        // Slide 2 2
        ctx.sp = sp;
        ctx.slide(2,2);
        // EnterCode _const
        ctx.jump("bluejelly.runtime.test.BarModule._const");
    }
    
    @JellyCode
    public void dummyTyCon(ExecutionContext ctx) {
        ctx.stackCheck(4);
        
        int sp;
        Node[] s = ctx.s;
        
        // x
        ctx.allocTyCon(0);
        // y
        ctx.allocTyCon(1);
        
        // x = TyCon0 1 y
        sp = ctx.sp;
        s[++sp] = Int.mkInt(1);
        s[sp+1] = s[sp-1]; ++sp;
        
        ctx.sp = sp;
        ctx.packTyCon(1, 2);
        
        // y = TyCon1 2 3
        sp = ctx.sp;
        s[++sp] = Int.mkInt(3);
        s[++sp] = Int.mkInt(2);
        
        ctx.sp = sp;
        ctx.packTyCon(0, 2);
        
        // PushVar 1
        sp = ctx.sp;        
        s[sp+1] = s[sp-1]; ++sp;
        // PushInt 0
        s[++sp] = Int.mkInt(0);
        // Slide 2 2
        ctx.sp = sp;
        ctx.slide(2,2);
        // EnterCode _const
        ctx.jump("bluejelly.runtime.test.BarModule._const");
    }
    
    @JellyCode
    public void papUpd(ExecutionContext ctx) {
        ctx.stackCheck(4);
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushCode _const
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule._const");
        // MkApp 2
        ctx.sp = sp;
        ctx.mkApp(2);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // PushCode id
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
        // MkApp 2
        ctx.sp = sp;
        ctx.mkApp(2);
        //PushInt 2
        sp = ctx.sp;
        s[++sp] = Int.mkInt(2);
        // PushVar 1
        s[sp+1] = s[sp-1]; ++sp;
        // Slide 2 2
        ctx.sp = sp;
        ctx.slide(2, 2);
        // Enter
        return;
    }
    
    @JellyCode
    public void whnfUpd(ExecutionContext ctx) {
        ctx.stackCheck(4);
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushCode _const
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule._const");
        // MkApp 2
        ctx.sp = sp;
        ctx.mkApp(2);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // mkTyCon 0 1
        ctx.sp = sp;
        ctx.mkTyCon(0, 1);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // PushCode id
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
        // MkApp 2
        ctx.sp = sp;
        ctx.mkApp(2);
        // Slide 1 2
        ctx.slide(1, 2);
        // Enter
        return;
    }
    
    @JellyCode(arity=2)
    public void primAddInt(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.evalVar(1, "bluejelly.runtime.test.BazModule.primAddInt$1");
        return;
    }
    
    @JellyCode
    public void primAddInt$1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.evalVar(1, "bluejelly.runtime.test.BazModule.primAddInt$2");
        return;
    }
    
    @JellyCode
    public void primAddInt$2(ExecutionContext ctx) {
        ctx.stackCheck(1);
        // UnboxInt 1
        int t1 = ((Int)ctx.getWhnf(1)).i;
        // UnboxInt 0
        int t2 = ((Int)ctx.getWhnf(0)).i;
        // AddInt
        int t3 = t1 + t2;
        // BoxInt
        ctx.slide(0,4);
        ctx.retInt(t3);
    }
    
    @JellyCode
    public void addInt(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushInt 2
        s[++sp] = Int.mkInt(2);
        // EnterCode primAddInt
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.test.BazModule.primAddInt");
    }
    
    @JellyCode
    public void updEval(ExecutionContext ctx) {
        ctx.stackCheck(5);
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushInt 2
        s[++sp] = Int.mkInt(2);
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // MkApp 3
        ctx.sp = sp;
        ctx.mkApp(3);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // PushVar 1
        s[sp+1] = s[sp-1]; ++sp;
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // MkApp 3
        ctx.sp = sp;
        ctx.mkApp(3);
        // PushVar 0
        sp = ctx.sp;
        s[sp+1] = s[sp]; ++sp;
        // PushVar 1
        s[sp+1] = s[sp-1]; ++sp;
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // Slide 1 2
        ctx.sp = sp;
        ctx.slide(3, 2);
        // Enter
        return;
    }
    
    @JellyCode
    public void caf(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushCode addInt
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.addInt");
        // PushCode addInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.addInt");
        // PushCode primAddInt
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // Return
        ctx.sp = sp;
        return;
    }
    
    @JellyCode
    public void quickSum(ExecutionContext ctx) {
        // Prologue
        ctx.stackCheck(3);
        Node[] s = ctx.s;
        int sp = ctx.sp;
        // PushInt 10
        // PushInt 11
        // PushCode primAddInt
        s[++sp] = Int.mkInt(10);
        s[++sp] = Int.mkInt(11);
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // Enter
        ctx.sp = sp;
        return;
    }
    
}
