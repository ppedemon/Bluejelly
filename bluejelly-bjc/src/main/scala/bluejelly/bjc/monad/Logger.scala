package bluejelly.bjc.monad

import scalaz._
import effect._
import syntax.monoid._
import syntax.monad._

import IO._

final class Logger[W,A](val runLogger: IORef[W] => IO[A]) {
  private[monad] def run(implicit F:Monoid[W]):IO[(A,W)] = for {
    r <- newIORef(mzero[W])
    a <- runLogger(r)
    w <- r.read
  } yield (a,w)

  def map[B](f: A => B):Logger[W,B] = 
    new Logger(runLogger(_) map f)

  def flatMap[B](f: A => Logger[W,B]):Logger[W,B] = 
    new Logger(r => runLogger(r) >>= (f(_).runLogger(r)))
}

object Logger extends LoggerInstances

sealed abstract class LoggerInstances {
  private[monad] type LoggerTF[W] = {
    type t[A] = Logger[W,A]
  }

  implicit def loggerMonadInstances[W](implicit F:Monoid[W]):MonadListen[LoggerTF[W]#t, W] 
      with MonadIO[LoggerTF[W]#t] 
      with MonadControlIO[LoggerTF[W]#t] = 

    new MonadListen[LoggerTF[W]#t, W] with MonadIO[LoggerTF[W]#t] with MonadControlIO[LoggerTF[W]#t] {
      def point[A](a: => A):Logger[W,A] = new Logger(_ => IO(a))
  
      override def map[A,B](logger:Logger[W,A])(f:A => B):Logger[W,B] = logger map f
  
      def bind[A,B](logger:Logger[W,A])(f:A => Logger[W,B]):Logger[W,B] = logger flatMap f
  
      def writer[A](w:W, v:A):Logger[W,A] = 
        new Logger(r => r.mod(_ |+| w) >> IO(v))

      def listen[A](ma:Logger[W,A]):Logger[W,(A,W)] = 
        new Logger(r => ma.run >>= Function.tupled((a,w) => r.mod(_ |+| w) >> IO((a,w))))

      def liftIO[A](ioa:IO[A]):Logger[W,A] = 
        new Logger(_ => ioa)

      def liftControlIO[A](f:RunInBase[LoggerTF[W]#t,IO] => IO[A]):Logger[W,A] = 
        new Logger(r => f(new RunInBase[LoggerTF[W]#t,IO] {
          def apply[B] = (logger:Logger[W,B]) => logger.runLogger(r) map (b => new Logger[W,B](_ => IO(b)))
      }))
    }
}
