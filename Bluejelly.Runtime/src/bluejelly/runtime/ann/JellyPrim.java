package bluejelly.runtime.ann;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Signals a language primitive with some name and arity. 
 * Used for generating primitive boilerplate code automatically.
 * 
 * @author ppedemon
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface JellyPrim {
  String name();
  int arity();
}
