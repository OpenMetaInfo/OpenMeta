package info.openmeta.starter.metadata.constant;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public interface MetadataConstant {

    String METADATA_UPGRADE_API = "/metadata/upgrade";

    /** Version control model mapping relationship between design time and runtime */
    Map<String, String> BASIC_METADATA_MODELS = ImmutableMap.<String, String>builder()
            .put("DesignModel", "SysModel")
            .put("DesignField", "SysField")
            .put("DesignModelIndex", "SysModelIndex")
            .put("DesignOptionSet", "SysOptionSet")
            .put("DesignOptionItem", "SysOptionItem")
            .build();
}
