package info.openmeta.framework.orm.meta;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * MetaOptionItem
 */
@Data
public class MetaOptionItem implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private Long appId;

    private Long optionSetId;

    private String optionSetCode;

    private Integer sequence;

    private String itemCode;

    private String itemName;

    private String parentItemCode;

    private String itemColor;

    private String description;

}