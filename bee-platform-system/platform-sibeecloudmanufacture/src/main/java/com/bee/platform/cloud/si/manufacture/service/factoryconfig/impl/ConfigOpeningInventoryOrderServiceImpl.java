package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventoryOrderDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventoryOrderDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventorySearchDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigOpeningInventoryOrder;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigOpeningInventoryOrderMapper;
import com.bee.platform.cloud.si.manufacture.entity.ConfigOpeningInventoryOrderDetail;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventoryOrderDetailRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventoryOrderRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventorySearchRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigOpeningInventoryOrderDetailService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigOpeningInventoryOrderService;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 期初库存主表 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigOpeningInventoryOrderServiceImpl extends ServiceImpl<ConfigOpeningInventoryOrderMapper, ConfigOpeningInventoryOrder> implements ConfigOpeningInventoryOrderService {

    @Autowired
    private ConfigOpeningInventoryOrderDetailService detailService;

    /**
     * 生成单号
     * @return 编号
     */
    @Override
    public String generateCode() {
        LocalDate now = LocalDate.now();
        String timeString = now.toString().replace("-", "");
        String mills = String.valueOf(Instant.now().toEpochMilli());
        String sequence = mills.substring(mills.length() - 6);

        //业务id
        StringBuffer code = new StringBuffer();
        // 期初单号
        code.append("QC");
        code.append(timeString);
        code.append(sequence);


        return code.toString();
    }

    /**
     * 条件查询期初库存
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @param page     分页对象
     * @return 期初库存信息
     */
    @Override
    public ResponseResult<List<ConfigOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(AuthPlatformUserInfo userInfo, ConfigOpeningInventorySearchRQ rq, Page page) {

        Pagination pagination = PageUtils.transFromPage(page);
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        Wrapper<ConfigOpeningInventoryOrder> wrapper = new EntityWrapper<ConfigOpeningInventoryOrder>().eq("deleted", 0);

        if (!ObjectUtils.isEmpty(enterpriseId)) {
            wrapper.eq("enterprise_id", enterpriseId);
        }
        if (!ObjectUtils.isEmpty(factoryId)) {
            wrapper.eq("factory_id", factoryId);
        }
        if (!ObjectUtils.isEmpty(rq.getCode())) {
            wrapper.like("code", rq.getCode());
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            wrapper.ge("opening_inventory_time", rq.getStartTime());
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.le("opening_inventory_time", rq.getEndTime());
        }

        List<ConfigOpeningInventoryOrder> list = baseMapper.selectPage(pagination, wrapper);
        List<ConfigOpeningInventorySearchDTO> dto = BeanUtils.assemble(ConfigOpeningInventorySearchDTO.class, list);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));


    }


    /**
     * 根据id查看期初库存详情
     *
     * @param id id
     * @return 期初库存详情
     */
    @Override
    public ConfigOpeningInventoryOrderDTO getOpeningInventoryById(Integer id) {

        // 查询主表详情
        ConfigOpeningInventoryOrder openingInventoryOrder = selectById(id);

        // 查询子表详请列表
        List<ConfigOpeningInventoryOrderDetail> orderDetails = detailService.selectList(new EntityWrapper<ConfigOpeningInventoryOrderDetail>()
                .eq("opening_inventory_order_id", id)
                .eq("deleted", Status.FALSE.getKey()));

        List<ConfigOpeningInventoryOrderDetailDTO> detailDTOS = BeanUtils.assemble(ConfigOpeningInventoryOrderDetailDTO.class, orderDetails);

        ConfigOpeningInventoryOrderDTO dto = BeanUtils.copyProperties(openingInventoryOrder, ConfigOpeningInventoryOrderDTO.class);
        dto.setDetailDTOS(detailDTOS);

        return dto;

    }


    /**
     * 保存期初库存详情
     * @param userInfo 用户详情
     * @param rq 期初库存信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOpeningInventoryOrder(AuthPlatformUserInfo userInfo, ConfigOpeningInventoryOrderRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        // 校验编号重复
        String code = rq.getCode();
        List<ConfigOpeningInventoryOrder> exist = selectList(new EntityWrapper<ConfigOpeningInventoryOrder>()
                .eq("code", code)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("单号编码重复，编码为:" + code);
            log.error("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ConfigOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.OPENING_INVENTORY_SAVE_FAILED_CODE_RE);
        }

        // 保存主表信息
        ConfigOpeningInventoryOrder openingInventoryOrder = BeanUtils.copyProperties(rq, ConfigOpeningInventoryOrder.class);
        openingInventoryOrder.setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);

        if (!insert(openingInventoryOrder) ){
            log.error("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ConfigOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.OPENING_INVENTORY_SAVE_FAILED);
        }

        Integer id = openingInventoryOrder.getId();
        // 遍历依次保存详情信息
        List<ConfigOpeningInventoryOrderDetailRQ> detailRQS = rq.getDetailRQS();
        List<ConfigOpeningInventoryOrderDetail> openingInventoryOrderDetails = BeanUtils.assemble(ConfigOpeningInventoryOrderDetail.class, detailRQS);
        openingInventoryOrderDetails.forEach(o->o.setOpeningInventoryOrderId(id).setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId).setCreateId(userId).setCreator(userName).setCreateTime(time));

        for (ConfigOpeningInventoryOrderDetail openingInventoryOrderDetail : openingInventoryOrderDetails) {
            detailService.saveOpeningInventoryOrderDetail(openingInventoryOrderDetail);
        }

        return id;
    }
}
