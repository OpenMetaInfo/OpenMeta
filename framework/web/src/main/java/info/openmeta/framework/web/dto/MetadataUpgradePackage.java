package info.openmeta.framework.web.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Metadata upgrade package
 */
@Data
@NoArgsConstructor
public class MetadataUpgradePackage implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String modelName;

    private List<Map<String, Object>> createRows = new ArrayList<>();

    private List<Map<String, Object>> updateRows = new ArrayList<>();

    private List<Serializable> deleteCodes = new ArrayList<>();
}
