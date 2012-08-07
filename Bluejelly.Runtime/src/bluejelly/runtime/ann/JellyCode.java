package bluejelly.runtime.ann;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation denoting that a method in a module holds code
 * compiled for some BlueJelly function.
 * 
 * @author ppedemon
 */

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface JellyCode {
	int arity() default 0;
	boolean matcher() default false;
}
