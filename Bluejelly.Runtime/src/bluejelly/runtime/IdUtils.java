package bluejelly.runtime;

/**
 * Trivial Id utilities.
 * @author ppedemon
 */
public final class IdUtils {
	
	public static final String qualify(Module m, String id) {
		return qualify(m.getClass().getName(), id);
	}
	
	public static final String qualify(String moduleName, String id) {
		return new StringBuilder()
			.append(moduleName).append('.').append(id).toString();
	}
	
	public static final String qualifier(String qid) {
		int ix = qid.lastIndexOf('.');
		if (ix < 0) {
			throw new IllegalArgumentException(
					"Attempt to unqualify invalid id: " + qid);
		}
		return qid.substring(0,ix);
	}
	
}
