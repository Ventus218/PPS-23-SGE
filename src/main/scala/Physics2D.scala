import Dimensions2D.*

object Physics2D:
  /** Gives colliders the methods to implement in order to detect the collisions
    * with other colliders
    */
  trait Collider extends Positionable with Scalable:
    /** Use AABB collision detection algorithm to detect a collision with a
      * Rectangle
      *
      * @param other
      * @return
      *   true if a detection is detected, false otherwise
      */
    def collides(other: RectCollider): Boolean

  /** Gives the capability to detect an AABB collision to a Behaviour. Width and
    * Height are scaled based on ScaleX and ScaleY of the Dimensions2D.Scalable
    * trait. The shape of this collider is a Rectangle. The center of the
    * collider is based on X and Y of Positionable.
    *
    * @param width
    *   width of the collider, must be greater than zero otherwise throws an
    *   IllegalArgumentException
    * @param height
    *   height of the collider, must be greater than zero otherwise throws an
    *   IllegalArgumentException
    */
  trait RectCollider(private var width: Double, private var height: Double)
      extends Collider:
    require(width > 0)
    require(height > 0)

    def colliderWidth: Double = width * scaleX
    def colliderHeight: Double = height * scaleY

    def colliderWidth_=(w: Double): Unit = if w > 0 then width = w
    def colliderHeight_=(h: Double): Unit = if h > 0 then height = h

    private def rightCorner: Double = x + width / 2
    private def bottomCorner: Double = y + height / 2
    private def leftCorner: Double = x - width / 2
    private def topCorner: Double = y - height / 2

    override def collides(other: RectCollider): Boolean =
      this.topCorner <= other.bottomCorner &&
        this.leftCorner <= other.rightCorner &&
        this.rightCorner >= other.leftCorner &&
        this.bottomCorner >= other.topCorner

  trait CircleCollider(private var r: Double) extends Collider:
    require(r > 0)

    def radius: Double = r
    def radius_=(radius: Double) = if radius > 0 then r = radius

    override def collides(other: RectCollider): Boolean = ???
