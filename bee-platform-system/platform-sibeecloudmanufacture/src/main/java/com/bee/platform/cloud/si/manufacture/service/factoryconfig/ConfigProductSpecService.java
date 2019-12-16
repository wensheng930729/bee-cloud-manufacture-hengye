package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductSpecDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductSpecParam;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSpecUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;

import java.util.List;

/**
 * <p>
 * 产品规格表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigProductSpecService extends IService<ConfigProductSpec> {

    /**
     * 修改产品规格信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     */
    void updateProductSpec(AuthPlatformUserInfo userInfo, ConfigProductSpecUpdateRQ rq);

    /**
     * 根据产品id查询产品规格列表
     * @param productId 产品id
     * @param userInfo 用户信息
     * @return 产品规格列表
     */
    List<ConfigProductSpecDTO> getProductSpecByProductId(Integer productId, AuthPlatformUserInfo userInfo);

    /**
     * @notes: 当前用户查询产品规格
     * @Author: junyang.li
     * @Date: 13:59 2019/11/26
     * @param param : 查询参数可以为空
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec>
     */
    List<ConfigProductSpec> getProductSpecByProductIds(ProductSpecParam param);
}
