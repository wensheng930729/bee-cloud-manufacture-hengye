package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigProductDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductListDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductTestAttributeInDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductTestAttributeOutDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProduct;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductTestItemUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import io.swagger.models.auth.In;

import java.util.List;

/**
 * <p>
 * 产品档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigProductService extends IService<ConfigProduct> {
    /**
     * 根据条件搜索产品列表
     * @param userInfo 用户信息
     * @param rq 搜索条件
     * @param page 分页对象
     * @return 产品列表
     */
    ResponseResult<List<ConfigProductListDTO>> searchProductList(AuthPlatformUserInfo userInfo, ConfigProductSearchRQ rq, Page page);

    /**
     * 保存产品信息
     * @param userInfo 用户信息
     * @param rq 产品信息
     * @return id
     */
    Integer saveProduct(AuthPlatformUserInfo userInfo, ConfigProductSaveRQ rq);

    /**
     * 修改产品信息
     * @param userInfo 用户信息
     * @param rq 产品信息
     * @return id
     */
    Integer updateProduct(AuthPlatformUserInfo userInfo, ConfigProductUpdateRQ rq);

    /**
     * 根据id删除产品信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteProductById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据id查看产品详情信息
     * @param userInfo 用户信息
     * @param id id
     * @return 产品详情信息
     */
    ConfigProductDTO getProductById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 修改产品化验项配置信息
     * @param userInfo 用户信息
     * @param rq 化验项信息
     * @return id
     */
    Integer updateTestItem(AuthPlatformUserInfo userInfo, ConfigProductTestItemUpdateRQ rq);

    /**
     * 根据产品id查询化验输入项列表
     * @param userInfo 用户信息
     * @param productId 产品id
     * @return 化验结果项列表
     */
    List<ConfigProductTestAttributeInDTO> getProductAttributeInByProductId(Integer productId, AuthPlatformUserInfo userInfo);
    /**
     * 根据产品id查询化验结果项列表
     * @param userInfo 用户信息
     * @param productId 产品id
     * @return 化验结果项列表
     */
    List<ConfigProductTestAttributeOutDTO> getProductAttributeOutByProductId(Integer productId, AuthPlatformUserInfo userInfo);

    /**
     * 根据产品类型查询产品列表
     * @param userInfo 用户信息
     * @param category 产品类别 0 全部 1 主料 2 辅料 3 成品 4 用户定义的类别  5 包含(主料 辅料)  6 包含(主料  成品) 7包含(辅料  成品)
     * @return 产品列表
     */
    List<ConfigProductListDTO> getProductListByCategory(AuthPlatformUserInfo userInfo, Integer category);

    /**
     * @Description 判断产品是否拥有化验项
     * @author chenxm66777123
     * @Date 2019/10/25 16:34
     * @version 1.0.0
     */
    boolean judgeHaveProductAttribute(Integer productId, AuthPlatformUserInfo userInfo);

    /**
     * @notes: 查询当前工厂所有的产品类别 ，暂时只返回产品id 和产品名称，如有需要可自行添加
     * @Author: junyang.li
     * @Date: 10:24 2019/11/26
     * @param userInfo : 当前操作人
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProduct>
     */
    List<ConfigProduct> getProductList(AuthPlatformUserInfo userInfo);

}
