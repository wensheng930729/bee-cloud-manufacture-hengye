package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProArtificialFeedMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProArtificialFeedDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductionOutStorageDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ProArtificialFeedRQ;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRawMaterialLossService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProArtificialFeedService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * <p>
 * 人工补料表 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Slf4j
@Service
public class ProArtificialFeedServiceImpl extends ServiceImpl<ProArtificialFeedMapper, ProArtificialFeed> implements ProArtificialFeedService {

    @Autowired
    private ConfigProductService productService;

    @Autowired
    private ConfigRepositoryService repositoryService;

    @Autowired
    private ConfigDeviceService deviceService;

    @Autowired
    private StorageService storageService;

    @Autowired
    private ProArtificialFeedMapper artificialFeedMapper;

    @Autowired
    private ConfigRawMaterialLossService rawMaterialLossService;

    /**
     * @descriptin 人工补料
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult artificialFeed(ProArtificialFeedRQ rq, AuthPlatformUserInfo userInfo) {
        ProArtificialFeed proArtificialFeed = BeanUtils.copyProperties(rq, ProArtificialFeed.class);
        proArtificialFeed.setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setCreateTime(new Date());
        //获取产品信息
        ConfigProduct product = productService.selectOne(new EntityWrapper<>(new ConfigProduct()
                .setId(rq.getProductId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(product)) {
            proArtificialFeed.setProductName(product.getName()).setUnit(product.getUnitValue());
            if (product.getCategoryId().equals(1)) {
                //主料
                proArtificialFeed.setType(0);
            } else if (product.getCategoryId().equals(2)){
                //辅料
                proArtificialFeed.setType(1);
            }
        }
        //获取产品所在仓库相关信息
        ConfigRepository repository = repositoryService.selectOne(new EntityWrapper<>(new ConfigRepository()
                .setId(rq.getWarehouseId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(repository)) {
            proArtificialFeed.setWarehouseName(repository.getName());
        }
        //获取矿热炉信息
        ConfigDevice device = deviceService.selectOne(new EntityWrapper<>(new ConfigDevice()
                .setId(rq.getFurnaceId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(device)) {
            proArtificialFeed.setFurnaceName(device.getName());
        }
        if (!this.insert(proArtificialFeed)) {
            log.error("人工补料失败 类：{} 方法：{}", "ProArtificialFeedServiceImpl", "artificialFeed");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        //获取产品损耗
        ConfigRawMaterialLoss loss = rawMaterialLossService
                .selectOne(new EntityWrapper<ConfigRawMaterialLoss>()
                        .eq("product_id", proArtificialFeed.getProductId())
                        .eq("deleted", Status.FALSE.getKey()));
        //扣减产品库存
        BigDecimal num = proArtificialFeed.getNum();
        if (!ObjectUtils.isEmpty(loss) && !ObjectUtils.isEmpty(loss.getLoss())) {
            num = num.multiply(new BigDecimal("1").add(loss.getLoss()));
        }
        ProductionOutStorageDetailDTO storage = new ProductionOutStorageDetailDTO();
        storage.setProductId(proArtificialFeed.getProductId())
                .setProductName(proArtificialFeed.getProductName())
                .setProductNumber(num)
                .setProductUnit(proArtificialFeed.getUnit())
                .setProductSpecId(proArtificialFeed.getProductSpecId())
                .setProductSpecName(proArtificialFeed.getProductSpecName())
                .setStorageId(proArtificialFeed.getWarehouseId())
                .setStorageName(proArtificialFeed.getWarehouseName());
        storageService.saveProductionOutStorage(storage, userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 查询人工补料记录列表
     * @author xin.huang
     * @param userInfo
     * @param pagination
     * @date 2019/9/25
     * @return
     */
    @Override
    public ResponseResult<List<ProArtificialFeedDTO>> findArtificialFeedList(AuthPlatformUserInfo userInfo, Pagination pagination) {
        List<ProArtificialFeed> proArtificialFeeds = artificialFeedMapper.selectPage(pagination, new EntityWrapper<>(new ProArtificialFeed()
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setStatus(Status.TRUE.getKey()))
                .orderBy("create_time", false));
        List<ProArtificialFeedDTO> artificialFeedList = BeanUtils.assemble(ProArtificialFeedDTO.class, proArtificialFeeds);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, artificialFeedList, PageUtils.transToPage(pagination));
    }

}
