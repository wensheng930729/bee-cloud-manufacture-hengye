package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigElectricityPriceCheckDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigElectricityPriceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigAmmeter;
import com.bee.platform.cloud.si.manufacture.entity.ConfigElectricityPrice;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigElectricityPriceMapper;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceCheckRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceUpdateCheckRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigElectricityPriceService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.javassist.runtime.Desc;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */

@Slf4j
@Service
public class ConfigElectricityPriceServiceImpl extends ServiceImpl<ConfigElectricityPriceMapper, ConfigElectricityPrice> implements ConfigElectricityPriceService {

    /**
     * 搜索电价管理列表
     *
     * @param userInfo 用户信息
     * @param page     分页对象
     * @return 电价列表
     */
    @Override
    public ResponseResult<List<ConfigElectricityPriceDTO>> searchElectricityPriceList(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigElectricityPrice> wrapper = new EntityWrapper<ConfigElectricityPrice>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        List<ConfigElectricityPrice> electricityPriceList = baseMapper.selectPage(pagination, wrapper);
        // 如果日期为2100-01-01 则设置为null
        Date date = new Date(200, 0, 1);
        electricityPriceList.forEach(o->{if(o.getExpirationDate().compareTo(date)==0){
            o.setExpirationDate(null);
        }});
        List<ConfigElectricityPriceDTO> dto = BeanUtils.assemble(ConfigElectricityPriceDTO.class, electricityPriceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 保存电价信息
     *
     * @param userInfo 用户信息
     * @param rq       电价信息
     * @return 电价id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveElectricityPrice(AuthPlatformUserInfo userInfo, ConfigElectricityPriceSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();


        // 如果没有结束日期  则定义为2100-01-01
        if (ObjectUtils.isEmpty(rq.getExpirationDate())) {
            rq.setExpirationDate(new Date(200, 0, 1));
        }
        // 校验开始日期时间 小于 结束日期时间
        if(rq.getEffectiveDate().after(rq.getExpirationDate()) || !rq.getStartTime().before(rq.getEndTime())){
            log.error("保存电价信息失败,电价起始日期时间大于等于结束日期时间,调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "saveElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_SAVE_FAILED_START_TIME_BIG);
        }

        // 校验电价区间重复
        ConfigElectricityPriceCheckRQ configElectricityPriceCheckRQ = BeanUtils.copyProperties(rq, ConfigElectricityPriceCheckRQ.class);

        List<ConfigElectricityPriceCheckDTO> checkDTOS = baseMapper.saveRepeatCheck(configElectricityPriceCheckRQ);

        if (!CollectionUtils.isEmpty(checkDTOS)) {
            log.error("保存电价信息失败,电价时间区间重复,调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "saveElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_SAVE_FAILED_TIME_RE);
        }

        ConfigElectricityPrice configElectricityPrice = BeanUtils.copyProperties(rq, ConfigElectricityPrice.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setDeleted(Status.FALSE.getKey())
                .setCreator(userName)
                .setCreateTime(time);
        // 保存电表信息
        if (!insert(configElectricityPrice)) {
            log.error("保存电价信息失败，调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "saveElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_SAVE_FAILED);

        }
        return configElectricityPrice.getId();
    }

    /**
     * 修改电价信息
     *
     * @param userInfo 用户信息
     * @param rq       电价信息
     * @return 电价id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateElectricityPrice(AuthPlatformUserInfo userInfo, ConfigElectricityPriceUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer id = rq.getId();

        // 如果没有结束日期  则定义为2100-01-01
        if (ObjectUtils.isEmpty(rq.getExpirationDate())) {
            rq.setExpirationDate(new Date(200, 0, 1));
        }

        // 校验开始日期时间 小于 结束日期时间
        if(rq.getEffectiveDate().after(rq.getExpirationDate()) || rq.getStartTime().after(rq.getEndTime())){
            log.error("保存电价信息失败,电价起始日期时间大于结束日期时间,调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "saveElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_UPDATE_FAILED_START_TIME_BIG);
        }
        // 校验电价区间重复
        ConfigElectricityPriceUpdateCheckRQ checkRQ = BeanUtils.copyProperties(rq, ConfigElectricityPriceUpdateCheckRQ.class);

        List<ConfigElectricityPriceCheckDTO> checkDTOS = baseMapper.updateRepeatCheck(checkRQ);

        if (!CollectionUtils.isEmpty(checkDTOS)) {
            log.error("保存电价信息失败,电价时间区间重复,调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "updateElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_UPDATE_FAILED_TIME_RE);
        }


        // 校验此id的电价数据是否存在
        List<ConfigElectricityPrice> have = selectList(new EntityWrapper<ConfigElectricityPrice>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改电价信息失败,此id电价不存在,调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "updateElectricityPrice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_UPDATE_FAILED_NO_DATA);
        }


        ConfigElectricityPrice configElectricityPrice = BeanUtils.copyProperties(rq, ConfigElectricityPrice.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 保存电表信息
        if (!updateById(configElectricityPrice)) {
            log.error("保存电价信息失败，调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "updateElectricityPrice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_UPDATE_FAILED);

        }
        return configElectricityPrice.getId();
    }

    /**
     * 根据id删除电价信息
     *
     * @param userInfo 用户信息
     * @param id       电价id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteElectricityPriceById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigElectricityPrice exist = selectOne(new EntityWrapper<ConfigElectricityPrice>().eq("id", id).eq("deleted", 0));
        // 验证是否存在电表信息
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id删除电表信息，没有找到相应数据，id为：" + id);
            return;
        }
        // 删除电表信息
        if (!updateById(new ConfigElectricityPrice().setId(id).setDeleted(1).setModifyId(userId).setModifier(userName).setModifyTime(time))) {
            log.error("删除电表信息失败，调用{}的{}方法出错", "ConfigElectricityPriceServiceImpl", "deleteElectricityPriceById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ELECTRICITY_PRICE_DELETED_FAILED);
        }
    }


    @Override
    public ConfigElectricityPriceDTO getElectricityPriceById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigElectricityPrice electricityPrice = selectOne(new EntityWrapper<ConfigElectricityPrice>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        // 如果日期为2100-01-01 则设置为null
        Date date = new Date(200, 0, 1);
        if(electricityPrice.getExpirationDate().compareTo(date)==0){
            electricityPrice.setExpirationDate(null);
        }

        return BeanUtils.copyProperties(electricityPrice, ConfigElectricityPriceDTO.class);
    }
}
