//Menu Item Cell Factory
package gridview;

import org.controlsfx.control.GridCell;
import org.controlsfx.control.GridView;

import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.layout.GridPane;
import javafx.util.Callback;

public class MenuItemCellFactory implements Callback<GridView<MenuItem>, GridCell<MenuItem>> {

    @Override
    public GridCell<MenuItem> call(GridView<MenuItem> listview) {
        return new MenuItemCell();
    }
}