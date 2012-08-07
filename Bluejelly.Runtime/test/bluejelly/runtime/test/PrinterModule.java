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
 * Test the print built-in function.
 * @author ppedemon
 */
public class PrinterModule implements Module {

    @JellyCode
    public void printWhnf(ExecutionContext ctx) {
        // Prologue
        ctx.stackCheck(1);
        Node[] s = ctx.s;
        int sp = ctx.sp;
        // PushInt 1
        s[++sp] = Int.mkInt(1);
        // EnterCode "bluejelly.runtime.Printer.print"
        ctx.sp = sp;
        ctx.jump("bluejelly.runtime.Printer.println");
    }
    
    @JellyCode
    public void printSimpleTyCon(ExecutionContext ctx) {
        // Prologue
        ctx.stackCheck(3);
        // allocTyCon
        ctx.allocTyCon(0);
        // PushInt 1
        // PushInt 2
        Node[] s = ctx.s;
        int sp = ctx.sp;
        s[++sp] = Int.mkInt(2);
        s[++sp] = Int.mkInt(1);
        // PackTyCon 
        ctx.sp = sp;
        ctx.packTyCon(0, 2);
        // EnterCode "bluejelly.runtime.Printer.print"
        ctx.jump("bluejelly.runtime.Printer.println");        
    }

    @JellyCode
    public void printComplexTyCon(ExecutionContext ctx) {
        // Prologue
        ctx.stackCheck(4);
        Node[] s = ctx.s;
        int sp = ctx.sp;
        // AllocTyCon
        ctx.allocTyCon(0);
        // AllocAp
        ctx.allocApp();
        // PushInt 2
        // PushInt 1
        // PushCode primAddInt
        sp = ctx.sp;
        s[++sp] = Int.mkInt(2);
        s[++sp] = Int.mkInt(1);
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // PackApp
        ctx.sp = sp;    
        ctx.packApp(0, 3);
        // PushVar 1
        sp = ctx.sp;
        s[sp+1] = s[sp];
        ++sp;
        // AllocAp
        ctx.sp = sp;
        ctx.allocApp();
        // PushInt 1
        // PushCode primAddInt
        sp = ctx.sp;
        s[++sp] = Int.mkInt(1);
        s[++sp] = ctx.getFun("bluejelly.runtime.test.BazModule.primAddInt");
        // PackApp
        ctx.sp = sp;
        ctx.packApp(0, 2);
        
        ctx.mkTyCon(0, 0);
        ctx.mkTyCon(0, 0);
        ctx.mkTyCon(1, 2);
        ctx.mkTyCon(3, 1);
        // PackTyCon 
        ctx.packTyCon(0, 4);
        // EnterCode "bluejelly.runtime.Printer.print"
        ctx.jump("bluejelly.runtime.Printer.println");
    }
    
    @JellyCode
    public void knot(ExecutionContext ctx) {
        ctx.stackCheck(3);
        ctx.allocTyCon(0);
        ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.packTyCon(0, 2);
    }
    
    @JellyCode
    public void printCyclic(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PrinterModule.knot");
        ctx.jump("bluejelly.runtime.Printer.println");
    }
    
    @JellyCode
    public void printEmptyList(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.mkTyCon(0, 0);
        ctx.jump("bluejelly.runtime.Printer.printListln");
    }

    @JellyCode
    public void printNonEmptyList(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.mkTyCon(0, 0);
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.mkTyCon(1, 2);
        ctx.jump("bluejelly.runtime.Printer.printListln");
    }

    @JellyCode
    public void ones(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PrinterModule.ones");
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.mkTyCon(1, 2);
    }
    
    @JellyCode 
    public void printOnes(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PrinterModule.ones");
        ctx.jump("bluejelly.runtime.Printer.printListln");
    }
    
}
