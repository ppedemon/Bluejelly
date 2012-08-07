package bluejelly.runtime;

import java.util.HashMap;
import java.util.Map;

import bluejelly.runtime.Config.OutStyle;
import bluejelly.runtime.nodes.Dict;
import bluejelly.runtime.nodes.Executable;
import bluejelly.runtime.nodes.NApp;
import bluejelly.runtime.nodes.Node;


/**
 * BlueJelly runtime. Most of the action takes place here.
 * 
 * @author ppedemon
 */
public class Runtime {
	
	// For command line processing
	private static final String SIZES = "kKmM";
	private static final String CMD_LINE_ARGS = "sSmMhvlb";
	private static final String VERSION = 
			"The Bluejelly Runtime System, v" + System.getProperty("app.version");
	private static final String USAGE = 
		"\nRuntime [options] function, where options are:\n" +
		" -mp <module_path> (like a java classpath)\n" +
		"  -l           print result as a list\n" +
	    "  -b           print result as a boolean\n" +
		"  -s <size>    initial stack size (default = 2K entries)\n" +
		"  -S <size>    maximum stack size (default = 4K entries)\n" +
		"  -m <size>    initial marker stack size (default = 1K entry)\n" +
		"  -M <size>    maximum marker stack size (default = 2K entries)\n\n" +
		"Size is an integer number followed by a unit specifier. \n" +
		"Unit specifiers are K,k,M, or m, default unit is K.\n";
	
	// Runtime configuration
	private final Config config;
	
	// Table of executable heap nodes
	private final Map<String,Executable> codeTable = 
		new HashMap<String,Executable>(31);	
	
	// Table of loaded dictionaries
	private final Map<String,Dict> dictTable =
		new HashMap<String,Dict>(31);
	
	// Module loader component
	private final ModuleLoader loader = new ModuleLoader(this);
	
	/**
	 * Default constructor. Use default {@link Config} values.
	 */
	public Runtime() {
		this(new Config());
	}
	
	/**
	 * Create a new instance with the given configuration.
	 * @param config    {@link Config} for this runtime instance
	 */
	public Runtime(Config config) {
		this.config = config;
	}
	
	/**
	 * Get runtime configuration.
	 * @return    runtime {@link Config}
	 */
	public Config getConfig() {
		return this.config;
	}
	
	/**
	 * Load the given module.
	 * 
	 * @param module    module to load
	 * @throws JellyException    if module can't be loaded
	 */
	public void load(String moduleName) throws JellyException {
		this.loader.load(moduleName);
	}

	/**
	 * Register an {@link Executable} function.
	 * 
	 * @param funId     function name
	 * @param e         {@link Executable} node for the function
	 */	
	protected void register(String funId, Executable e) {
		this.codeTable.put(funId, e);
	}

	/**
	 * Get the {@link Executable} node for the given function id.
	 * We ensure thread-safe usage of the loader by making this
	 * method synchronized.
	 * 
	 * @param funId    function id whose code we want to retrieve
	 * @return         intended code node
	 */
	public synchronized Executable getFun(String funId) {
		Executable e = this.codeTable.get(funId);
		if (e != null) {
			return e;
		}
		String moduleName = IdUtils.qualifier(funId); 
		if (this.loader.isLoaded(moduleName)) {
			throw new JellyRuntimeException("No definition for: " + funId);
		} else {
			this.loadIfNecessary(moduleName);
			return getFun(funId);
		}
	}

	/**
	 * Register a {@link Dict} in the runtime.
	 * 
	 * @param dictId    dictionary name
	 * @param d         {@link Dict} node to register
	 */
	protected void register(String dictId, Dict d) {
		this.dictTable.put(dictId, d);
	}
	
	/**
	 * Get a {@link Dict} node for the given dictionary id.
	 * We ensure thread-safe usage of the loader by making this
	 * method synchronized.
	 *  
	 * @param dictId    id of the {@link Dict} node to retrieve
	 * @return          intended {@link Dict} node
	 */
	public synchronized Dict getDict(String dictId) {
		String moduleName = IdUtils.qualifier(dictId); 
		this.loadIfNecessary(moduleName);
		Dict d = this.dictTable.get(dictId);
		if (d == null) {
			throw new JellyRuntimeException("No dictionary named: " + dictId);
		} else {
			return d;
		}
	}
	
	/**
	 * Evaluate a function with the given Id in a new {@link ExecutionContext}.
	 * Module defining the function must have been loaded first.
	 * 
	 * @param funId    id of function to evaluate.
	 * 
	 * @return 
	 *   a {@link Future} that will make available the 
	 *   {@link ExecutionContext} where the evaluation took place
	 */
	public Future<ExecutionContext> eval(String funId) {
		Executable e = this.getFun(funId);
		ExecutionContext ctx = new ExecutionContext(this);
		return ctx.eval(funId, e);
	}
	
	/**
	 * Eval the given node in a new {@link ExecutionContext}. Any code
	 * referenced by the module must have been loaded first.
	 * 
	 * @param name    name identifying the execution context
	 * @param n       {@link Node} to evaluate
	 * 
	 * @return 
	 *   a {@link Future} that will make available the 
	 *   {@link ExecutionContext} where the evaluation took place
	 */
	public Future<ExecutionContext> eval(String name, Node n) {
		ExecutionContext ctx = new ExecutionContext(this);
		return ctx.eval(name, n);
	}
	
	// Convenient wrapper to load call
	private void loadIfNecessary(String moduleName) {
		if (this.loader.isLoaded(moduleName)) {
			return;
		}
		try {
			this.loader.load(moduleName);
		} catch (JellyException e) {
			throw new JellyRuntimeException(e);
		}
	}
	
	// -------------------------------------------------------------------
	// Parse command line. Yes, by hand.
	// -------------------------------------------------------------------
	
	/**
	 * Parse command line, modifying settings in the given
	 * {@link Config} object. Return the name of the function
	 * to execute. If there is no function to execute or if the
	 * command line is invalid in any way, we abort the program.
	 *  
	 * @param args    command line arguments
	 * @param c       {@link Config} to fill-in
	 * @return        function to execute
	 */
	private static String parseCmdLine(String[] args, Config c) {
		char lastOpt = 0;
		String fun = null;
		
		for (String s: args) {
			if (s.charAt(0) == '-') {
				if (s.length() != 2) {
					invalidCmdLine("Invalid option: " + s);
					return null;
				}
				char chr = s.charAt(1);
				int i = CMD_LINE_ARGS.indexOf(chr);
				if (i == -1) {
					invalidCmdLine("Invalid option: " + s);
					return null;
				}
				switch (chr) {
				case 'h': 
					System.out.println(USAGE);
					System.exit(0);
				case 'v':
					System.out.println(VERSION);
					System.exit(0);
				case 'l':
					c.style = OutStyle.LIST;
					break;
				case 'b':
					c.style = OutStyle.BOOL;
					break;
				default:
					lastOpt = chr;
					break;
				}
			} else {
				switch (lastOpt) {
				case 's': 
					c.initStackSize = parseSize(s); lastOpt = 0; break;
				case 'S': 
					c.maxStackSize = parseSize(s);  lastOpt = 0; break;
				case 'm': 
					c.initMarkerStackSize = parseSize(s); lastOpt = 0; break;
				case 'M': 
					c.maxMarkerStackSize = parseSize(s); lastOpt = 0; break;
				case 0:
					if (fun == null) {
						fun = s;
					} else {
						invalidCmdLine("Multiple functions to execute:" + 
							s + " and " + fun);
						return null;
					}
				}
			}
		}
		if (lastOpt != 0) {
			invalidCmdLine("Missing argument for option: -" + lastOpt);
			return null;
		}
		if (fun == null) {
			invalidCmdLine("Missing function to execute");
			return null;
		}
		c.maxStackSize = Math.max(
				c.maxStackSize, 
				c.initStackSize);
		c.maxMarkerStackSize = Math.max(
				c.maxMarkerStackSize, 
				c.initMarkerStackSize);		
		return fun;
	}
	
	private static void invalidCmdLine(String msg) {
		System.err.println(msg);
		System.err.println("Use -h for help");
		System.exit(1);
	}
	
	private static int parseSize(String s) {
		int mult = 10;
		char last = s.charAt(s.length() - 1);
		if (SIZES.indexOf(last) != -1) {
			if (last == 'M' || last == 'm') {
				mult = 20;
			}
			s = s .substring(0, s.length() - 1);
		} 
		try {
			int size = Integer.parseInt(s);
			return size << mult;
		} catch (NumberFormatException e) {
			invalidCmdLine("Invalid size specification: " + s);
			return 0;
		}
	}
	
	/**
	 * Entry point: for a given function f, evaluate print(f)
	 * @param args               name of the function to execute
	 * @throws JellyException    if execution fails
	 */
	public static void main(String[] args) throws JellyException {
		Config c = new Config();
		String funId = parseCmdLine(args, c);
		Runtime r = new Runtime(c);

		String printFun = "bluejelly.runtime.Printer.";
		switch (c.style) {
		case DEFAULT: printFun += "println"       ; break;
		case LIST:    printFun += "printListln"   ; break;
		case BOOL:    printFun += "printBooleanln"; break;
		}
		
		NApp app = new NApp(new Node[] {r.getFun(funId), r.getFun(printFun)});
		Future<ExecutionContext> f = r.eval("<toplevel>",app);
		f.get();
	}

}
