//Menu Item Cell
package gridview;

import org.controlsfx.control.GridCell;

import javafx.scene.control.ListCell;
import javafx.scene.layout.GridPane;

public class MenuItemCell extends GridCell<MenuItem> {

    @Override
    protected void updateItem(MenuItem item, boolean empty) {
        // TODO Auto-generated method stub
        super.updateItem(item, empty);
        if (empty || item == null) {
            setText(null);
            setGraphic(null);
        } else {
            setText(item.getName().toString());
            setStyle("-fx-background-color: #ffffff; -fx-background-radius: 15; -fx-border-radius: 15; -fx-border-width: 0; -fx-padding: 10; -fx-pref-width: 145; -fx-max-width: 145; -fx-max-width: 145; -fx-pref-height: 130; -fx-max-height: 130; -fx-effect: dropshadow(three-pass-box, #93948d, 10, 0, 0, 0);");
        }
    }
}