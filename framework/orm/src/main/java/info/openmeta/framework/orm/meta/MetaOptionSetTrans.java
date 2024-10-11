package info.openmeta.framework.orm.meta;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * MetaOptionSetTrans object
 */
@Data
public class MetaOptionSetTrans implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private String languageCode;

    private Long rowId;

    private String name;

    private String description;
}