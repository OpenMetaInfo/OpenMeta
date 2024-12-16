package info.openmeta.starter.designer.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * DTO for changed rows of a model
 */
@Data
@NoArgsConstructor
public class ModelChangesDTO {

    private String modelName;
    private List<RowChangeDTO> createdRows = new ArrayList<>();
    private List<RowChangeDTO> updatedRows = new ArrayList<>();
    private List<RowChangeDTO> deletedRows = new ArrayList<>();

    public ModelChangesDTO(String modelName) {
        this.modelName = modelName;
    }

    public void addCreatedRow(RowChangeDTO rowChangeDTO) {
        createdRows.add(rowChangeDTO);
    }

    public void addUpdatedRow(RowChangeDTO rowChangeDTO) {
        updatedRows.add(rowChangeDTO);
    }

    public void addDeletedRow(RowChangeDTO rowChangeDTO) {
        deletedRows.add(rowChangeDTO);
    }

}