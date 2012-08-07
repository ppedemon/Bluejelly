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
 * Dummy module, for testing puposes only.
 * 
 * @author ppedemon
 */
public class BarModule implements Module {
    
    @JellyCode(arity=2)
    public void swap(ExecutionContext ctx) {        
        ctx.stackCheck(2);
        // PushVar 0
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[sp+1] = s[sp]; ++sp;
        // PushVar 2
        s[sp+1] = s[sp-2]; ++sp;
        // Slide 2 2
        ctx.sp = sp;
        ctx.slide(2,2);
        // enter
        return;
    }
    
    @JellyCode(arity=2)
    public void _const(ExecutionContext ctx) {
        ctx.stackCheck(1);
        // PushVar 0
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[sp+1] = s[sp]; ++sp;
        // Slide 1 2
        ctx.sp = sp;
        ctx.slide(1,2);
        // Enter
        return;
    }
    
    @JellyCode
    public void whnfTest(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushInt
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushCode _const
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BarModule._const");
        // Enter
        ctx.sp = sp;
        return;
    }
    
    @JellyCode
    public void intMatch(ExecutionContext ctx) {
        ctx.stackCheck(3);
        // PushCont 
        ctx.pushCont("bluejelly.runtime.test.BarModule.intMatch$1");
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
        // Enter
        return;
    }
    
    @JellyCode(matcher=true)
    public void intMatch$1(ExecutionContext ctx) {
        // MatchInt
        switch(ctx.getInt()) {
        case 0: 
        {
            ctx.altBasicPrologue();
            ctx.stackCheck(1);
            ctx.retInt(1);
            break;
        }
        case 3: 
        {
            ctx.altBasicPrologue();
            ctx.stackCheck(1);
            ctx.retInt(4);
            break;
        }
        }
    }
    
    @JellyCode
    public void tycon1(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushCont
        ctx.pushCont("bluejelly.runtime.test.BarModule.tyconMatch");
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushInt 2
        s[++sp] = Int.mkInt(2);
        // retTyCon 0 2
        ctx.sp = sp;
        ctx.retTyCon(0, 2);
        // Return
        return;
    }

    @JellyCode
    public void tycon2(ExecutionContext ctx) {
        ctx.stackCheck(2);
        // PushCont
        ctx.pushCont("bluejelly.runtime.test.BarModule.tyconMatch");
        // PushInt 1
        int sp = ctx.sp;
        Node[] s = ctx.s;
        s[++sp] = Int.mkInt(1);
        // PushInt 2
        s[++sp] = Int.mkInt(2);
        // retTyCon 1 2
        ctx.sp = sp;
        ctx.retTyCon(1, 2);
        // Return
        return;
    }

    @JellyCode(matcher=true) 
    public void tyconMatch(ExecutionContext ctx) {
        // MatchCon
        switch(ctx.getTag()) {
        case 0:
        {
            // EnterCode inc
            ctx.altTyConPrologue();
            ctx.jump("bluejelly.runtime.test.BazModule.primAddInt");
            break;
        }
        default: 
        {
            ctx.defTyConPrologue();
            ctx.stackCheck(1);
            // PushInt -1
            int sp = ctx.sp;
            Node[] s = ctx.s;
            s[++sp] = Int.mkInt(-1);
            // Slide 1 1
            ctx.sp = sp;
            ctx.slide(1, 1);
            break;
        }    
        }
    }
    
    @JellyCode
    public void mkTyCon(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.s[++ctx.sp] = Int.mkInt(2);
        ctx.retTyCon(0, 2);
    }

    @JellyCode
    public void mkTyCon1(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.s[++ctx.sp] = Int.mkInt(2);
        ctx.retTyCon(1, 2);
    }

    @JellyCode
    public void trickyMatch(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.BarModule.trickyMatch$1");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.BarModule.mkTyCon");
    }

    @JellyCode
    public void trickyMatch1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.pushCont("bluejelly.runtime.test.BarModule.trickyMatch$1");
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.BarModule.mkTyCon1");
    }

    // Not a matcher continuation!
    @JellyCode
    public void trickyMatch$1(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;

        switch(ctx.getTag()) {
        case 0:
        {
            ctx.altTyConPrologue();
            ctx.stackCheck(1);
            ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 2]; ++ctx.sp;
            ctx.slide(1, 3);
            break;
        }
        default: 
        {
            ctx.defTyConPrologue();
            ctx.stackCheck(1);
            ctx.s[++ctx.sp] = Int.mkInt(-1);
            ctx.slide(1, 2);
            break;
        }    
        }        
    }
}
