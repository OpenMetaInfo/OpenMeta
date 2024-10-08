package info.openmeta.framework.orm.meta;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * MetaOptionItemTrans object
 */
@Data
public class MetaOptionItemTrans implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private String languageCode;

    private Long rowId;

    private String itemName;

    private String description;
}