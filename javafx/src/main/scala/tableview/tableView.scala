package tableview

import java.net.URL
import java.util.ResourceBundle

import javafx.application.Application
import javafx.beans.property.{SimpleDoubleProperty, SimpleIntegerProperty, SimpleStringProperty}
import javafx.beans.value.ObservableValue
import javafx.collections.{FXCollections, ObservableList}
import javafx.fxml._
import javafx.scene.control.{TableColumn, TableView}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.util.Callback

import scala.collection.JavaConversions
import scala.util.Random
import scala.util.control.NonFatal

/**
  * Shows a way to use a JavaFX TableView with Scala
  */
object TableViewExample {
  def main(args: Array[String]) {
    Application.launch(classOf[TableViewApp], args: _*)
  }
}

/**
  * Setup for the javafx app
  */
class TableViewApp extends javafx.application.Application {

  val loader = new FXMLLoader(getClass.getResource("TableView.fxml"))

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("TableView Example App")
      loader.load[Parent]()
      stage.setScene(new Scene(loader.getRoot[Parent]))
      stage.show()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }

}

/**
  * domain object
  */
case class Article(id: Int, name: String, price: Double)

/**
  * domain object, but usable with javafx
  */
class MutableArticle {

  val idProperty: SimpleIntegerProperty   = new SimpleIntegerProperty()
  val nameProperty: SimpleStringProperty  = new SimpleStringProperty()
  val priceProperty: SimpleDoubleProperty = new SimpleDoubleProperty()

  def setId(id: Int) = idProperty.set(id)

  def setName(name: String) = nameProperty.set(name)

  def setPrice(price: Double) = priceProperty.set(price)
}

/**
  * companion object to get a better initialisation story
  */
object MutableArticle {

  def apply(a: Article): MutableArticle = {
    val ma = new MutableArticle
    ma.setId(a.id)
    ma.setName(a.name)
    ma.setPrice(a.price)
    ma
  }

}

/**
  * util functions to bridge the javafx / scala gap
  */
object JfxUtils {

  type TCDF[S, T] = TableColumn.CellDataFeatures[S, T]

  import JavaConversions._

  def mkObservableList[T](collection: Iterable[T]): ObservableList[T] = {
    FXCollections.observableList(new java.util.ArrayList[T](collection))
  }

  private def mkCellValueFactory[S, T](
      fn: TCDF[S, T] => ObservableValue[T]): Callback[TCDF[S, T], ObservableValue[T]] = {
    new Callback[TCDF[S, T], ObservableValue[T]] {
      def call(cdf: TCDF[S, T]): ObservableValue[T] = fn(cdf)
    }
  }

  def initTableViewColumnCellValueFactory[S, T](tc: TableColumn[S, T], f: S => Any): Unit = {
    tc.setCellValueFactory(mkCellValueFactory(cdf => f(cdf.getValue).asInstanceOf[ObservableValue[T]]))
  }

}

/**
  * simulates a database for example
  */
object DataSource {

  val data =
    (1 to 1000) map {
      case i => Article(i, s"name $i", Random.nextDouble() * i)
    }

}

/**
  * populates data in the table view controller
  */
class TableViewAppController extends Initializable {

  import JfxUtils._

  type ArticleTC[T] = TableColumn[MutableArticle, T]

  @FXML var tableView: TableView[MutableArticle] = _

  @FXML var columnId: ArticleTC[Int]       = _
  @FXML var columnName: ArticleTC[String]  = _
  @FXML var columnPrice: ArticleTC[Double] = _

  val mutableArticles = mkObservableList(DataSource.data.map(MutableArticle(_)))

  /**
    * provide a table column and a generator function for the value to put into
    * the column.
    *
    * @tparam T the type which is contained in the property
    * @return
    */
  def initTableViewColumn[T]: (ArticleTC[T], (MutableArticle) => Any) => Unit =
    initTableViewColumnCellValueFactory[MutableArticle, T]

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    tableView.setItems(mutableArticles)

    initTableViewColumn[Int](columnId, _.idProperty)
    initTableViewColumn[String](columnName, _.nameProperty)
    initTableViewColumn[Double](columnPrice, _.priceProperty)

  }

}
