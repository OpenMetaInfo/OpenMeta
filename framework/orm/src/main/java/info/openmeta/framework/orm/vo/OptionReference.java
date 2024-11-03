package info.openmeta.framework.orm.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Option reference object.
 * Used to reference the option item code, name, and color.
 */
@Data
public class OptionReference implements Serializable  {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Option Item Code")
    private String itemCode;

    @Schema(description = "Option Item Name")
    private String itemName;

    @Schema(description = "Option Item Color")
    private String itemColor;

    /**
     * Create an OptionReference object.
     *
     * @param itemCode Option item code
     * @param itemName Option item name
     * @return OptionReference object
     */
    static public OptionReference of(String itemCode, String itemName) {
        OptionReference optionReference = new OptionReference();
        optionReference.setItemCode(itemCode);
        optionReference.setItemName(itemName);
        return optionReference;
    }

    /**
     * Create an OptionReference object.
     *
     * @param itemCode Option item code
     * @param itemName Option item name
     * @param itemColor Option item color
     * @return OptionReference object
     */
    static public OptionReference of(String itemCode, String itemName, String itemColor) {
        OptionReference optionReference = OptionReference.of(itemCode, itemName);
        optionReference.setItemColor(itemColor);
        return optionReference;
    }
}
