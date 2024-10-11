package info.openmeta.framework.orm.meta;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * MetaFieldTrans object
 */
@Data
public class MetaFieldTrans implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private String languageCode;

    private Long rowId;

    private String labelName;

    private String description;
}