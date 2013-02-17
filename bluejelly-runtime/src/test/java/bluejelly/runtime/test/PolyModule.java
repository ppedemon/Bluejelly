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
import bluejelly.runtime.nodes.Double;
import bluejelly.runtime.nodes.Int;

/**
 * Dummy module for checking dictionary usage.
 * @author ppedemon
 */
public class PolyModule implements Module {
    
    /*
     * Pretend we have this:
     * 
     * module PolyModule where
     * 
     * class C a where f :: a -> a
     * instance C Double where f _ = PI
     * instance C Int where f x = 1 + x
     * 
     * poly :: C a => a -> a
     * poly = f
     * 
     * polyInt = poly 41
     * polyDbl = poly 1.0
     */
    
    /*
     * Get dictionary's `f' function:
     *   f d = case d of C x -> x
     */
    @JellyCode(arity=2)
    public void f(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.evalVar(0, "bluejelly.runtime.test.PolyModule.f$1");
    }
    
    @JellyCode(matcher=true)
    public void f$1(ExecutionContext ctx) {
        int tag = ctx.getTag();
        switch(tag) {
        case 0: 
            ctx.altTyConPrologue();
            ctx.slide(1,1);
        }
    }
    
    /*
     * Definition of `f' for C Int instance:
     * f_CInt x = 1 + x
     */
    @JellyCode(arity=1)
    public void $fCInt(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;
        ctx.s[++ctx.sp] = Int.mkInt(1);
        ctx.slide(2,1);
        ctx.jump("bluejelly.Int.add");
    }

    /*
     * Definition of `f' for C Double instance:
     * f_CInt x = 1 + x
     */
    @JellyCode(arity=1)
    public void $fCDbl(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.slide(0,1);
        ctx.retDouble(Math.PI);
    }
    
    /*
     * Dictionary for C Int instance.
     */
    @JellyCode
    public void $instCInt(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PolyModule.$fCInt");
        ctx.retTyCon(0, 1);
    }

    /*
     * Dictionary for C Double instance.
     */
    @JellyCode
    public void $instCDbl(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PolyModule.$fCDbl");
        ctx.retTyCon(0, 1);
    }

    /*
     * poly :: C a => a -> a
     * poly = f
     */
    @JellyCode(arity=2)
    public void poly(ExecutionContext ctx) {
        ctx.stackCheck(1);
        ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;
        ctx.slide(1,1);
        ctx.jump("bluejelly.runtime.test.PolyModule.f");
    }
    
    /*
     * polyInt = poly 41
     */
    @JellyCode
    public void polyInt(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[++ctx.sp] = Int.mkInt(41);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PolyModule.$instCInt");
        ctx.jump("bluejelly.runtime.test.PolyModule.poly");
    }

    /*
     * polyDbl = poly 1.0
     */
    @JellyCode
    public void polyDbl(ExecutionContext ctx) {
        ctx.stackCheck(2);
        ctx.s[++ctx.sp] = new Double(1.0);
        ctx.s[++ctx.sp] = ctx.getFun("bluejelly.runtime.test.PolyModule.$instCDbl");
        ctx.jump("bluejelly.runtime.test.PolyModule.poly");
    }    
}
