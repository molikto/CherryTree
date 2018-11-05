
package utils

import java.sql.{JDBCType, PreparedStatement, ResultSet}
import java.util.UUID

import com.github.tminglei.slickpg._
import slick.ast.FieldSymbol
import slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}


trait UUIDPlainImplicits {

  implicit val uuidGetResult: GetResult[UUID] = GetResult(_.nextObject().asInstanceOf[UUID])

  implicit object SetUUID extends SetParameter[UUID] {
    def apply(v: UUID, pp: PositionedParameters) {
      pp.setObject(v, JDBCType.BINARY.getVendorTypeNumber)
    }
  }

}


trait MyPostgresProfile extends ExPostgresProfile
  with PgArraySupport
  with PgDate2Support
  with PgPlayJsonSupport
  with PgNetSupport
  with PgLTreeSupport
  with PgRangeSupport
  //with PgHStoreSupport
  with PgSearchSupport {

  override val pgjson = "jsonb"
  ///
  override val api = new API with ArrayImplicits
    with DateTimeImplicits
    with PlayJsonImplicits
    with NetImplicits
    with LTreeImplicits
    with RangeImplicits
    //with HStoreImplicits
    with SearchImplicits
    with SearchAssistants {}

  val plainApi = new API with ByteaPlainImplicits
    with SimpleArrayPlainImplicits
    with Date2DateTimePlainImplicits
    with PlayJsonPlainImplicits
    with SimpleNetPlainImplicits
    with SimpleLTreePlainImplicits
    with SimpleRangePlainImplicits
    with UUIDPlainImplicits
    //with SimpleHStorePlainImplicits
    with SimpleSearchPlainImplicits {

  }

}


object MyPostgresProfile extends MyPostgresProfile