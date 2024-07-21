package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.base.enums.SystemUser;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.SwitchUser;
import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.metadata.service.MetadataUpgradeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Metadata Upgrade controller
 */
@Slf4j
@Tag(name = "Metadata Upgrade API")
@RestController
public class MetadataUpgradeController {

    @Autowired
    private MetadataUpgradeService metadataUpgradeService;

    /**
     * Upgrades the metadata of multiple models, all within a single transaction
     * to avoid refreshing the model pool repeatedly and missing dependency data.
     *
     * @param metadataPackages the metadata packages to upgrade
     * @return Success or not
     */
    @Operation(summary = "Metadata Upgrade API")
    @PostMapping("/metadata/upgrade")
    @SwitchUser(value = SystemUser.INTEGRATION_USER)
    public ApiResponse<Boolean> releasePackage(@RequestBody List<MetadataUpgradePackage> metadataPackages) {
        Assert.notEmpty(metadataPackages, "Metadata upgrade data must not be empty!");
        metadataUpgradeService.upgradeMetadata(metadataPackages);
        return ApiResponse.success(true);
    }
}