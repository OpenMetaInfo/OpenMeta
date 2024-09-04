package info.openmeta.framework.orm.meta;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Translation Item
 */
@Data
public class TranslationItem implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String languageCode;

    private String modelName;

    private String fieldName;

    private String rowId;

    private String original;

    private String translation;
}