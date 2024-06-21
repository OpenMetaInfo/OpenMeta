package info.openmeta.framework.web.controller;


import info.openmeta.framework.base.enums.SystemRole;
import info.openmeta.framework.web.service.CacheService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.RequireRole;
import info.openmeta.framework.web.response.ApiResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@Tag(name = "Cache API")
@RestController
@RequestMapping("/cache")
public class CacheController {

    @Autowired
    private CacheService service;

    /**
     * Query the cache for the specified key.
     *
     * @param key cache key
     * @return the cached value
     */
    @Operation(summary = "searchCache", description = "Query cache based on the specified key")
    @GetMapping(value = "/{key}")
    @Parameter(name = "key", description = "The key to query")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Object> search(@PathVariable("key") String key) {
        Assert.notBlank(key, "The cache key cannot be empty: {0}", key);
        Map<String, Object> objects = service.search(Collections.singletonList(key));
        return ApiResponse.success(objects.get(key));
    }

    /**
     * Batch queries the cache.
     * @param keys the keys to query
     * @return The cache map of key-value
     */
    @Operation(summary = "searchList: Batch query caches", description = "Query multiple caches based on the keys")
    @GetMapping(value = "/searchList")
    @Parameter(name = "keys", description = "The keys to query")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Map<String, Object>> searchList(@RequestParam List<String> keys) {
        Assert.allNotNull(keys, "The cache keys cannot contains null: {0}", keys);
        return ApiResponse.success(service.search(keys));
    }

    /**
     * Batch deletes caches.
     *
     * @param keys the cache keys to delete
     * @return delete count
     */
    @Operation(summary = "deleteList: Batch delete caches", description = "Delete multiple caches based on the keys")
    @DeleteMapping(value = "/deleteList")
    @Parameter(name = "keys", description = "The keys of the caches to delete.")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Long> deleteList(@RequestParam List<String> keys) {
        Assert.allNotNull(keys, "The deleted cache keys cannot contains null: {0}", keys);
        return ApiResponse.success(service.delete(keys));
    }

}
