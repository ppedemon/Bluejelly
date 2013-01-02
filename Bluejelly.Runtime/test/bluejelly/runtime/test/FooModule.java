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
import bluejelly.runtime.nodes.Str;

/**
 * Dummy module, for testing purposes only.
 * 
 * @author ppedemon
 */
public class FooModule implements Module {
    
    public void ignoreMe() {}
    
    @JellyCode(arity=1)
    public void id(ExecutionContext ctx) {
        // enter
        return;
    }    
    
    @JellyCode(arity=1)
    public void inc(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.evalVar(0, "bluejelly.runtime.test.FooModule.inc$1");
        return;
    }
    
    @JellyCode
    public void inc$1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        // UnboxInt
        int t1 = ((Int)ctx.getWhnf(0)).i;
        // PushUInt 1
        int t2 = 1;
        // AddInt
        int t3 = t1 + t2;
        // BoxInt
        ctx.slide(0,2);
        ctx.retInt(t3);
        return;
    }
    
    @JellyCode
    public void main(ExecutionContext ctx) {
        ctx.stackCheck(2);
                
        // PushCode id
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id"); 
        // PushCode id
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");         
        // EnterCode swap
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.test.BarModule.swap");
    }
    
    @JellyCode
    public void test(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushInt 42
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(42);
        // PushCode id
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
        // PushCode inc
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.inc");
        // EnterCode const
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.test.BarModule._const");
    }
    
    @JellyCode
    public void nApp(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushInt 0
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(0);
        // PushCode id
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
        // nap 2
        ctx.sp = sp;
        ctx.mkNApp(2);
        // PushInt 1
        sp = ctx.sp;
        s[++sp] = Int.mkInt(1);
        // PushCode inc
        s[++sp] = ctx.getFun("bluejelly.runtime.test.FooModule.inc");
        // nap 2
        ctx.sp = sp;
        ctx.mkNApp(2);
        // EnterCode(const)
        ctx.jump("bluejelly.runtime.test.BarModule._const");
        return;
    }
    
    @JellyCode
    public void app(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushCode swap
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule.swap");
        // ap 1
        ctx.sp = sp;
        ctx.mkApp(1);
        // PushInt 1
        sp = ctx.sp;
        s[++sp] = Int.mkInt(1);
        // PushVar 1
        s[sp+1] = s[sp-1]; ++sp;
        // Slide 2 1
        ctx.sp = sp;
        ctx.slide(2, 1);
        // Enter
        return;
    }
    
    @JellyCode
    public void bigIntArith(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.FooModule.bigInt$1");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.BigInt.zero");
    }
    
    @JellyCode
    public void bigInt$1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.FooModule.bigInt$2");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.BigInt.one");
    }

    @JellyCode
    public void bigInt$2(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.pushCont("bluejelly.runtime.test.FooModule.bigInt$3");
        ctx.s[++ctx.sp] = new Str("335385764583214543694543532");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.BigInt.fromStr");
    }

    @JellyCode
    public void bigInt$3(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.BigInt.add");
        ctx.mkApp(3);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.BigInt.add");
    }

    @JellyCode
    public void torture(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.FooModule.torture$1");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
    }
    
    @JellyCode
    public void torture$1(ExecutionContext ctx) {
        ctx.slide(0,1);
        ctx.retInt(1);
        return;
    }
    
    @JellyCode(arity=1)
    public void torture1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.FooModule.torture1$1");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.FooModule.id");
    }

    @JellyCode
    public void torture1$1(ExecutionContext ctx) {
        ctx.slide(0,2);
        ctx.retInt(1);
        return;
    }
    
    @JellyCode
    public void startTorture1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = Int.mkInt(3);
        ctx.jump("bluejelly.runtime.test.FooModule.torture1");
    }
}
