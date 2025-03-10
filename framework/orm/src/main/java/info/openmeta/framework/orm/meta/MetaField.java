package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.MaskingType;
import lombok.Data;
import org.springframework.util.StringUtils;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * MetaField object
 */
@Data
public class MetaField implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private Long appId;

    private String labelName;

    private String fieldName;

    private String columnName;

    private String modelName;

    private Long modelId;

    private String description;

    private FieldType fieldType;

    private String optionSetCode;

    private String relatedModel;

    private String relatedField;

    private String jointModel;

    private String jointLeft;

    private String jointRight;

    private String cascadedField;

    // Field level filters used by frontend
    private String filters;

    // Special values: `now` for Date and DateTime fields. Ignore case.
    private String defaultValue;

    // Memory compute attribute: Instantiated object of the default value.
    private Object defaultValueObject;

    private Integer length;

    private Integer scale;

    private boolean required;

    private boolean readonly;

    private boolean hidden;

    private boolean translatable;

    private boolean noncopyable;

    private boolean unsearchable;

    private boolean computed;

    private String expression;

    /**
     * Memory compute attribute: The fields in the expression or cascadedField.
     * ComputedField scenario: field1 + field2 + field3 -> [field1, field2, field3]
     * CascadedField scenario: field1.field2 -> [field1, field2]
     */
    private List<String> dependentFields;

    private boolean dynamic;

    private boolean encrypted;

    private MaskingType maskingType;

    /**
     * Get translation by language code from translations.
     * If the translation is not found, return the original name.
     *
     * @return label name
     */
    public String getLabelName() {
        String languageCode = ContextHolder.getContext().getLanguage().getCode();
        MetaFieldTrans labelTrans = TranslationCache.getFieldTrans(languageCode, id);
        if (labelTrans == null) {
            return labelName;
        } else {
            String translation = labelTrans.getLabelName();
            return StringUtils.hasText(translation) ? translation : labelName;
        }
    }
}