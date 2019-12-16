package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigProductSpecMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductSpecDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductSpecParam;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSpecRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSpecUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductSpecService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * 产品规格表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@Service
public class ConfigProductSpecServiceImpl extends ServiceImpl<ConfigProductSpecMapper, ConfigProductSpec>
        implements ConfigProductSpecService {


    @Autowired
    private ConfigProductService productService;

    @Autowired
    private ConfigProductSpecMapper configProductSpecMapper;

    /**
     * 修改产品规格信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 产品id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateProductSpec(AuthPlatformUserInfo userInfo, ConfigProductSpecUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<ConfigProductSpecRQ> productSpecList = rq.getProductSpecList();
        // 校验合格线数量不大于1
        List<ConfigProductSpecRQ> qualified = productSpecList.stream().filter(o -> o.getQualifiedLine().equals(Status.TRUE.getKey())).collect(Collectors.toList());
        if(qualified.size()>1){
            log.error("修改产品规格失败，合格线数量大于1,调用{}的{}方法出错", "ConfigProductSpecServiceImpl", "updateProductSpec()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_SPEC_FAILED_SIZE_TOO_BIG);
        }
        // 校验排序是否重复
        Set<Integer> sort = productSpecList.stream().map(o -> o.getSort()).collect(Collectors.toSet());
        if(productSpecList.size()>sort.size()){
            log.error("修改产品规格失败，排序重复,调用{}的{}方法出错", "ConfigProductSpecServiceImpl", "updateProductSpec()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_SPEC_FAILED_SORT_RE);

        }

        List<ConfigProductSpec> productSpecs = BeanUtils.assemble(ConfigProductSpec.class, productSpecList);
        productSpecs.forEach(o->{
            if(ObjectUtils.isEmpty(o.getId())){
                o.setEnterpriseId(enterpriseId).setFactoryId(factoryId).setCreateId(userId).setCreator(userName).setCreateTime(time);
            }else {
                o.setModifyId(userId).setModifier(userName).setModifyTime(time);
            }
        });

        if(!insertOrUpdateBatch(productSpecs)){
            log.error("修改产品规格失败，调用{}的{}方法出错", "ConfigProductSpecServiceImpl", "updateProductSpec()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_SPEC_FAILED);
        }


    }

    /**
     * 根据产品id查询产品规格列表
     * @param productId 产品id
     * @param userInfo 用户信息
     * @return 产品规格列表
     */
    @Override
    public List<ConfigProductSpecDTO> getProductSpecByProductId(Integer productId, AuthPlatformUserInfo userInfo) {
        List<ConfigProductSpec> productSpecs = selectList(new EntityWrapper<ConfigProductSpec>()
                .eq("product_id",productId)
                .eq("deleted",Status.FALSE.getKey())
                .eq("enterprise_id",userInfo.getOrgId())
                .orderBy("sort")
                .eq("factory_id",userInfo.getFactoryId()));

        return BeanUtils.assemble(ConfigProductSpecDTO.class, productSpecs);

    }

    /**
     * @notes: 当前用户查询产品规格
     * @Author: junyang.li
     * @Date: 13:59 2019/11/26
     * @param param : 查询参数可以为空
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProductSpec>
     */
    @Override
    public List<ConfigProductSpec> getProductSpecByProductIds(ProductSpecParam param) {
        if(param == null){
            param=new ProductSpecParam();
        }
        return configProductSpecMapper.getProductSpecByProductIds(param);
    }
}
