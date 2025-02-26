package info.openmeta.framework.orm.domain;

import lombok.Data;

import java.io.Serializable;

/**
 * Row data file info.
 * One file field may have multiple files according to the fieldType.
 */
@Data
public class DataFileInfo {

    private Serializable rowId;

    private String fieldName;

    private FileInfo fileInfo;
}
