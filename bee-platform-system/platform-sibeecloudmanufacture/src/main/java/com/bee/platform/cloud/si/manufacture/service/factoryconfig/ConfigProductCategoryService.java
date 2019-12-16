package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigProductCategoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductCategory;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategoryUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 产品类别 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigProductCategoryService extends IService<ConfigProductCategory> {
    /**
     * 搜索产品类别列表
     * @param userInfo 用户信息
     * @param rq 产品名称
     * @param page 分页对象
     * @return 产品分类列表
     */
    ResponseResult<List<ConfigProductCategoryDTO>> searchProductCategoryList(AuthPlatformUserInfo userInfo, ConfigProductCategorySearchRQ rq, Page page);

    /**
     * 保存产品类别
     * @param userInfo 用户信息
     * @param rq 产品类别信息
     * @return id
     */
    Integer saveProductCategory(AuthPlatformUserInfo userInfo, ConfigProductCategorySaveRQ rq);

    /**
     * 修改产品类别
     * @param userInfo 用户信息
     * @param rq 产品类别信息
     * @return id
     */
    Integer updateProductCategory(AuthPlatformUserInfo userInfo, ConfigProductCategoryUpdateRQ rq);

    /**
     * 根据id删除产品类别信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteProductCategoryById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 获取产品类别列表 下拉使用
     * @param userInfo 用户信息
     * @return 产品类别列表
     */
    List<ConfigProductCategoryDTO> getProductCategoryList(AuthPlatformUserInfo userInfo);

    ConfigProductCategoryDTO getProductCategoryById(AuthPlatformUserInfo userInfo, Integer id);
}
