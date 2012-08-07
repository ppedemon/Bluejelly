package bluejelly.runtime;

import static bluejelly.Bools.FALSE;
import bluejelly.runtime.ann.JellyCode;
import bluejelly.runtime.nodes.App;
import bluejelly.runtime.nodes.Int;
import bluejelly.runtime.nodes.NApp;
import bluejelly.runtime.nodes.Node;
import bluejelly.runtime.nodes.Str;
import bluejelly.runtime.nodes.TyCon;
import bluejelly.runtime.nodes.Updatable;

/**
 * Module providing a built-in print function.
 * @author ppedemon
 */
public class Printer implements Module {

	public static final char OPEN = '{';
	public static final char CLOSE = '}';
	public static final char SEP = ',';
	
	/**
	 * Default constructor.
	 */
	public Printer() {
	}	

	// -------------------------------------------------------------------
	// Print function follows. Print mechanism traverses node 
	// graph tail recursively, so you can print "tail" cyclic 
	// graphs in constant stack space.
	// -------------------------------------------------------------------
	
	/**
	 * Built-in println function.
	 * @see Printer#print(ExecutionContext)
	 * @param ctx   {@link ExecutionContext} for this method
	 */
	@JellyCode(arity=1)
	public void println(ExecutionContext ctx) {
		ctx.pushCont("bluejelly.runtime.Printer.println$1");
		ctx.jump("bluejelly.runtime.Printer.print");
	}
	
	@JellyCode
	public void println$1(ExecutionContext ctx) {
		System.out.println();
	}
	
	/**
	 * Built-in print function. Expect a single cell on the stack,
	 * which is the value to be printed. We will be forcing evaluation
	 * until printing the whole contents of the cell.
	 * 
	 * @param ctx   {@link ExecutionContext} for this method
	 */
	@JellyCode(arity=1)
	public void print(ExecutionContext ctx) {
		ctx.stackCheck(3);
		ctx.s[++ctx.sp] = Int.mkInt(0);
		ctx.s[++ctx.sp] = Int.mkInt(0);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 2]; ++ctx.sp;
		ctx.slide(3,1);
		ctx.jump("bluejelly.runtime.Printer.print$1");
		return;
	}

	@JellyCode
	public void print$1(ExecutionContext ctx) {
		ctx.stackCheck(1);
		ctx.evalVar(0, "bluejelly.runtime.Printer.print$2");
	}
	
	@JellyCode
	public void print$2(ExecutionContext ctx) {
		Node n = ctx.getWhnf();
		if (n instanceof Updatable) {
			Updatable u = (Updatable)n;
			if (u.isUpdated()) {
				printNode(ctx, u.getInd());
			}
		} else {
			printNode(ctx, n);
		}
	}
	
	/*
	 * Precondition: node is not an updated updatable.
	 */
	private void printNode(ExecutionContext ctx, Node n) {
		ctx.stackCheck(1);
		int d = ((Int)ctx.getWhnf(3)).i;
		int p = ((Int)ctx.getWhnf(2)).i;

		if (n instanceof App) {
			System.out.print("A" + OPEN);
			printNodes(ctx, ((App)n).getNodes());
		} 
		else if (n instanceof NApp) {
			// NApp of a single node don't need to be reduced
			NApp napp = (NApp)n;
			if (napp.getNodes().length == 1) {
				System.out.print(napp.getNodes()[0]);
				if (p == 1) System.out.print(repeat(d, CLOSE));
				ctx.slide(0, 4);
				ctx.retTyCon(0, 0);
			} else {
				System.out.print("N" + OPEN);
				printNodes(ctx, ((NApp)n).getNodes());
			}
		} 
		else if (n instanceof TyCon) {
			TyCon tycon = (TyCon)n;
			System.out.printf("T[%d]" + OPEN, tycon.getTag());
			// No children, avoid recursion
			if (tycon.getNodes().length == 0 ) { 
				System.out.print(CLOSE);
				if (p == 1) System.out.print(repeat(d, CLOSE));
				ctx.slide(0, 4);
				ctx.retTyCon(0, 0);
			} else {
				printNodes(ctx, tycon.getNodes());
			}
		} 
		else {
			System.out.print(n);
			if (p == 1) System.out.print(repeat(d, CLOSE));
			ctx.slide(0, 4);
			ctx.retTyCon(0, 0);
		}
	}
	
	/*
	 * Helper function: print a sequence of nodes.
	 */
	private void printNodes(ExecutionContext ctx, Node[] nodes) {
		ctx.stackCheck(4);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 3]; ++ctx.sp;
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 3]; ++ctx.sp;
		ctx.s[++ctx.sp] = Int.mkInt(nodes.length);
		ctx.s[++ctx.sp] = new TyCon(0, nodes);
		ctx.slide(4,4);
		ctx.jump("bluejelly.runtime.Printer.printOne");
	}
	
	@JellyCode(arity=2)
	public void printOne(ExecutionContext ctx) {
		ctx.stackCheck(3);
		Int n = (Int)ctx.s[ctx.sp - 1];
		Int d = (Int)ctx.s[ctx.sp - 3];
		Node[] nodes = ((TyCon)ctx.getWhnf()).getNodes();
		if (n.i == 1) {
			ctx.s[++ctx.sp] = Int.mkInt(d.i + 1);
			ctx.s[++ctx.sp] = Int.mkInt(1);
			ctx.s[++ctx.sp] = nodes[n.i - 1];
			ctx.slide(3,4);
		} else {
			ctx.s[++ctx.sp] = Int.mkInt(0);
			ctx.s[++ctx.sp] = Int.mkInt(0);
			ctx.s[++ctx.sp] = nodes[n.i - 1];
			ctx.pushCont("bluejelly.runtime.Printer.printOne$1");
		}
		ctx.jump("bluejelly.runtime.Printer.print$1");
	}
	
	@JellyCode
	public void printOne$1(ExecutionContext ctx) {
		int n = ((Int)ctx.s[ctx.sp - 2]).i;
		if (n > 1) {
			System.out.print(SEP);
		}
		ctx.stackCheck(4);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 4]; ++ctx.sp;
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 4]; ++ctx.sp;
		ctx.s[++ctx.sp] = Int.mkInt(n - 1);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 4]; ++ctx.sp;
		ctx.slide(4, 5);
		ctx.jump("bluejelly.runtime.Printer.printOne");
	}
	
	public String repeat(int n, char c) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < n; i++) sb.append(c);
		return sb.toString();
	}
	
	// -------------------------------------------------------------------
	// Assume tycons are lists and print them in a stylized way. 
	// This function is (like print) tail recursive, so you can 
	// use it for infinite lists.
	// -------------------------------------------------------------------

	@JellyCode(arity=1)
	public void printListln(ExecutionContext ctx) {
		ctx.pushCont("bluejelly.runtime.Printer.printListln$1");
		ctx.jump("bluejelly.runtime.Printer.printList");
	}

	/**
	 * Print a node as a list, with a trailing newline character.
	 * @see Printer#printList(ExecutionContext)
	 * @param ctx    {@link ExecutionContext} for this function
	 */
	@JellyCode
	public void printListln$1(ExecutionContext ctx) {
		System.out.println();
	}
	
	/**
	 * This function assumed the node to print is a list, and
	 * treats it specially.
	 * @param ctx   {@link ExecutionContext} for this function
	 */
	@JellyCode(arity=1)
	public void printList(ExecutionContext ctx) {
		System.out.print('[');
		ctx.stackCheck(2);
		ctx.s[++ctx.sp] = Int.mkInt(0);
		ctx.evalVar(1, "bluejelly.runtime.Printer.printList$1");
	}
		
	@JellyCode(matcher=true)
	public void printList$1(ExecutionContext ctx) {
		switch(ctx.getTag()) {
		// 0 -> []
		case 0:
			System.out.print(']');
			ctx.altTyConPrologue();
			ctx.slide(0,2);
			ctx.retTyCon(0, 0);
			break;
		// 1 -> (x:xs)
		case 1:
			ctx.altTyConPrologue();
			if (((Int)ctx.getWhnf(2)).i != 0) System.out.print(',');
			ctx.stackCheck(1);
			ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;
			ctx.pushCont("bluejelly.runtime.Printer.printTail");
			ctx.jump("bluejelly.runtime.Printer.print");
			break;
		}
	}
	
	@JellyCode(arity=1)
	public void printTail(ExecutionContext ctx) {
		ctx.stackCheck(2);
		ctx.s[++ctx.sp] = Int.mkInt(1);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 3]; ++ctx.sp;
		ctx.slide(2,4);
		ctx.pushCont("bluejelly.runtime.Printer.printList$1");
	}
	
	// -------------------------------------------------------------------
	// Assume tycons are booleans and print them in a stylized way. 
	// -------------------------------------------------------------------

	@JellyCode(arity=1)
	public void printBooleanln(ExecutionContext ctx) {
		ctx.pushCont("bluejelly.runtime.Printer.printBooleanln$1");
		ctx.jump("bluejelly.runtime.Printer.printBoolean");
	}
	
	@JellyCode
	public void printBooleanln$1(ExecutionContext ctx) {
		System.out.println();
	}
	
	@JellyCode(arity=1)
	public void printBoolean(ExecutionContext ctx) {
		ctx.pushCont("bluejelly.runtime.Printer.printBoolean$1");
	}
	
	@JellyCode
	public void printBoolean$1(ExecutionContext ctx) {
		ctx.stackCheck(1);
		TyCon t = (TyCon)ctx.getWhnf();
		if (t.getTag() == FALSE) {
			System.out.print("False");
		} else {
			System.out.print("True");
		}
		ctx.retTyCon(0, 0);
	}
	
	// -------------------------------------------------------------------
	// Show (i.e., turn node into string). Since strings are strict,
	// this is not useful for circular node graphs.
	// -------------------------------------------------------------------
	
	/**
	 * Built-in showln function.
	 * @see Printer#show(ExecutionContext)
	 * @param ctx   {@link ExecutionContext} for this method
	 */
	@JellyCode(arity=1)
	public void showln(ExecutionContext ctx) {
		ctx.pushCont("bluejelly.runtime.Printer.showln$1");
		ctx.jump("bluejelly.runtime.Printer.show");
	}
	
	@JellyCode
	public void showln$1(ExecutionContext ctx) {
		ctx.s[ctx.sp] = new Str(ctx.getWhnf().toString() + '\n');
	}

	/**
	 * Built-in show function. Expect a single cell on the stack,
	 * which is the value to be transformed into a string. We will 
	 * be forcing evaluation until printing the whole contents of 
	 * the cell.
	 * 
	 * <p>Since strings are strict, this is not useful for printing
	 * infinite structures.
	 * 
	 * @param ctx   {@link ExecutionContext} for this method
	 */
	@JellyCode(arity=1)
	public void show(ExecutionContext ctx) {
		ctx.stackCheck(1);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp]; ++ctx.sp;
		ctx.pushCont("bluejelly.runtime.Printer.show$1");
		return;
	}

	@JellyCode
	public void show$1(ExecutionContext ctx) {
		Node n = ctx.getWhnf();
		if (n instanceof Updatable) {
			Updatable u = (Updatable)n;
			if (u.isUpdated()) {
				showNode(ctx, u.getInd());
			}
		} else {
			showNode(ctx, n);
		}
	}
	
	public void showNode(ExecutionContext ctx, Node n) {
		if (n instanceof App) {
			showNodes("A", ctx, ((App)n).getNodes());
		} else if (n instanceof NApp) {
			// NApp of a single node must not be reduced
			NApp napp = (NApp)n;
			if (napp.getNodes().length == 1) {
				ctx.slide(0, 2);
				ctx.retStr(napp.getNodes()[0].toString());
			} else {
				showNodes("N", ctx, ((NApp)n).getNodes());
			}
		} else if (n instanceof TyCon) {
			TyCon tycon = (TyCon)n;
			String pref = "T[" + tycon.getTag() + "]";
			showNodes(pref, ctx, tycon.getNodes());
		} else {
			ctx.slide(0, 2);
			ctx.retStr(n.toString());
		}
	}
	
	public void showNodes(String pref, ExecutionContext ctx, Node[] nodes) {
		ctx.stackCheck(3);
		ctx.s[++ctx.sp] = Int.mkInt(nodes.length);
		ctx.s[++ctx.sp] = new TyCon(0, nodes);
		ctx.s[++ctx.sp] = new Str(pref + OPEN);
		ctx.slide(3,2);
		ctx.jump("bluejelly.runtime.Printer.showOne");
	}
	
	@JellyCode(arity=3)
	public void showOne(ExecutionContext ctx) {
		ctx.stackCheck(1);
		Int n = (Int)ctx.s[ctx.sp - 2];
		if (n.i == 0) {
			ctx.s[ctx.sp + 1] = new Str(ctx.getWhnf().toString() + CLOSE); ++ctx.sp;
			ctx.slide(1,3);
		} else {
			ctx.pushCont("bluejelly.runtime.Printer.showOne$1");
			Node curr = ((TyCon)ctx.s[ctx.sp - 1]).getNodes()[n.i - 1];
			ctx.s[++ctx.sp] = curr;
			ctx.jump("bluejelly.runtime.Printer.show");
		}
	}
	
	@JellyCode
	public void showOne$1(ExecutionContext ctx) {
		StringBuilder sb = new StringBuilder(ctx.getWhnf(1).toString());
		int n = ((Int)ctx.s[ctx.sp - 3]).i;
		if (n < ((TyCon)ctx.s[ctx.sp - 2]).getNodes().length) {
			sb.append(SEP);
		}
		sb.append(ctx.getWhnf(0).toString());
		
		ctx.stackCheck(3);
		ctx.s[++ctx.sp] = Int.mkInt(n - 1);
		ctx.s[ctx.sp + 1] = ctx.s[ctx.sp - 3]; ++ctx.sp;
		ctx.s[++ctx.sp] = new Str(sb.toString());		
		ctx.slide(3, 4);
		ctx.jump("bluejelly.runtime.Printer.showOne");
	}

}
