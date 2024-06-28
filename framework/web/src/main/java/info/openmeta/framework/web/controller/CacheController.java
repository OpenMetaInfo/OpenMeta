package info.openmeta.framework.web.controller;


import info.openmeta.framework.base.enums.SystemRole;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.RequireRole;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.framework.web.service.CacheService;
import io.swagger.v3.oas.annotations.Hidden;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@Hidden
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
    @GetMapping(value = "/{key}")
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
    @GetMapping(value = "/searchList")
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
    @DeleteMapping(value = "/deleteList")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Long> deleteList(@RequestParam List<String> keys) {
        Assert.allNotNull(keys, "The deleted cache keys cannot contains null: {0}", keys);
        return ApiResponse.success(service.delete(keys));
    }

}
