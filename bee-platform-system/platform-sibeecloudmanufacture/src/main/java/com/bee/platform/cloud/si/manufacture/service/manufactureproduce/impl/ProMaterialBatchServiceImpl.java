package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProMaterialBatchMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StorageInventoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.MaterialBatchDTO;
import com.bee.platform.cloud.si.manufacture.dto.MaterialBatchDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.MaterialBatchDetailRQ;
import com.bee.platform.cloud.si.manufacture.rq.MaterialBatchRQ;
import com.bee.platform.cloud.si.manufacture.rq.MaterialQueryRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.cloud.si.manufacture.service.PlcRealDataService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProIngredientRecordService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProIngredientStatisticService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProMaterialBatchDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProMaterialBatchService;
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
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 料批基本信息主表 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
@Slf4j
@Service
public class ProMaterialBatchServiceImpl extends ServiceImpl<ProMaterialBatchMapper, ProMaterialBatch> implements ProMaterialBatchService {

    @Autowired
    private ProMaterialBatchDetailService materialBatchDetailService;

    @Autowired
    private ProMaterialBatchMapper materialBatchMapper;

    @Autowired
    private ConfigProductService productService;

    @Autowired
    private ConfigRepositoryService repositoryService;

    @Autowired
    private ConfigPlcDeviceService configPlcDeviceService;

    @Autowired
    private PlcFieldConfigService plcFieldConfigService;

    @Autowired
    private PlcRealDataService plcRealDataService;

    @Autowired
    private ProIngredientStatisticService ingredientStatisticService;

    @Autowired
    private StorageInventoryMapper storageInventoryMapper;

    @Autowired
	private ProIngredientRecordService ingredientRecordService;

    /**
     * @descriptin 新增料批
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/24
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addMaterialBatch(MaterialBatchRQ rq, AuthPlatformUserInfo userInfo) {
        //校验料批名是否重复
        List<ProMaterialBatch> existMaterials = this.selectList(new EntityWrapper<>(new ProMaterialBatch()
                .setMaterialName(rq.getMaterialName())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(existMaterials)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.MATERIAL_BATCH_EXISTED);
        }

        //校验仓库中是否有相关产品
        if (!CollectionUtils.isEmpty(rq.getMaterialBatchDetailList())) {
            for (MaterialBatchDetailRQ detail : rq.getMaterialBatchDetailList()) {
                Integer count = storageInventoryMapper.selectCount(new EntityWrapper<>(new StorageInventory()
                                .setProductId(detail.getProductId())
                                .setProductSpecId(detail.getProductSpecId())
                                .setStorageId(detail.getWarehouseId())
                                .setStatus(Status.TRUE.getKey())
                                .setOrgId(userInfo.getOrgId())
                                .setFactoryId(userInfo.getFactoryId())));
                if (count <= 0) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.STORAGE_PRODUCT_NOT_EXISTED);
                }
            }
        }

        ProMaterialBatch proMaterialBatch = BeanUtils.copyProperties(rq, ProMaterialBatch.class);
        proMaterialBatch.setCreateTime(new Date())
                .setCreateId(userInfo.getId())
                .setActive(rq.getActive())
                .setFactoryId(userInfo.getFactoryId())
                .setCompanyId(userInfo.getOrgId());
        //获取成品相关信息
        ConfigProduct finishProduct = productService.selectOne(new EntityWrapper<>(new ConfigProduct()
                .setId(rq.getFinishProductId())
                .setStatus(Status.TRUE.getKey())));
        if (!ObjectUtils.isEmpty(finishProduct)) {
            proMaterialBatch.setFinishProductName(finishProduct.getName())
                    .setFinishUnit(finishProduct.getUnitValue());
        }
        //获取plc设备信息
        if (!ObjectUtils.isEmpty(rq.getPlcId())) {
            //禁用原有配置的plc料批
            this.update(new ProMaterialBatch().setActive(Status.FALSE.getKey()),
                    new EntityWrapper<>(new ProMaterialBatch()
                            .setPlcId(rq.getPlcId())
                            .setCompanyId(userInfo.getOrgId())));

            ConfigPlcDevice configPlcDevice = configPlcDeviceService.selectOne(new EntityWrapper<>(new ConfigPlcDevice()
                    .setId(rq.getPlcId())
                    .setStatus(Status.TRUE.getKey())));
            if (!ObjectUtils.isEmpty(configPlcDevice)) {
                proMaterialBatch.setPlcName(configPlcDevice.getName());
            }

        }

        //新增料批基本信息
        if (!this.insert(proMaterialBatch)) {
            log.error("添加料批基本信息失败 类：{} 方法：{}", "ProMaterialBatchServiceImpl", "addMaterialBatch");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }

        //新增料批明细信息
        if (!CollectionUtils.isEmpty(rq.getMaterialBatchDetailList())) {
            List<ProMaterialBatchDetail> materialDetails = BeanUtils.assemble(ProMaterialBatchDetail.class, rq.getMaterialBatchDetailList());
            //用于统计下料实时数据
            List<ProIngredientStatistic> statistics = new ArrayList<>(materialDetails.size());
            //用于统计配料实时数据
            List<ProIngredientRecord> records = new ArrayList<>(materialDetails.size());

            materialDetails.forEach(materialDetail -> {
                ConfigProduct product = productService.selectOne(new EntityWrapper<>(new ConfigProduct()
                        .setId(materialDetail.getProductId())
                        .setStatus(Status.TRUE.getKey())));
                if (!ObjectUtils.isEmpty(product)) {
                    materialDetail.setProductName(product.getName()).setUnit(product.getUnitValue());
                }
                //获取产品所在仓库信息
                ConfigRepository repository = repositoryService.selectOne(new EntityWrapper<>(new ConfigRepository()
                        .setId(materialDetail.getWarehouseId())
                        .setStatus(Status.TRUE.getKey())));
                if (!ObjectUtils.isEmpty(repository)) {
                    materialDetail.setWarehouseName(repository.getName());
                }

                //获取出料斗信息
                PlcFieldConfig plcFieldConfig = plcFieldConfigService.selectOne(new EntityWrapper<>(new PlcFieldConfig()
                        .setField(materialDetail.getPlcField())
                        .setStatus(Status.TRUE.getKey())
                        .setDeleted(Status.FALSE.getKey())));
                if (!ObjectUtils.isEmpty(plcFieldConfig)) {
                    materialDetail.setPlcFieldName(plcFieldConfig.getFieldName());
                }
                materialDetail.setBatchId(proMaterialBatch.getId()).setCreateTime(new Date());

                //配料统计信息
                if (!ObjectUtils.isEmpty(rq.getPlcId())) {
                    //实时下料数据基本信息
                    ProIngredientStatistic statistic = BeanUtils.copyProperties(materialDetail, ProIngredientStatistic.class);
                    statistic.setBatchId(proMaterialBatch.getId())
                            .setMaterialName(proMaterialBatch.getMaterialName())
                            .setPlcId(proMaterialBatch.getPlcId())
                            .setPlcName(proMaterialBatch.getPlcName())
                            .setFinishProductId(proMaterialBatch.getFinishProductId())
                            .setNum(BigDecimal.ZERO)
                            .setCreateTime(new Date());
                    statistics.add(statistic);

                    //配料实时数据统计
                    ProIngredientRecord record = BeanUtils.copyProperties(statistic, ProIngredientRecord.class);
                    record.setNum(BigDecimal.ZERO).setCreateTime(new Date())
                            .setCompanyId(userInfo.getOrgId())
                            .setFactoryId(userInfo.getFactoryId());
                    records.add(record);
                }
            });

            if (!materialBatchDetailService.insertBatch(materialDetails)) {
                log.error("添加料批明细信息失败 类：{} 方法：{}", "ProMaterialBatchServiceImpl", "addMaterialBatch");
                return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
            }

            //新增plc下料实时统计基本信息
            if (!ObjectUtils.isEmpty(statistics)) {
                ingredientStatisticService.insertBatch(statistics);
            }

            //新增配料实时统计基本信息
            if (!ObjectUtils.isEmpty(records)) {
                ingredientRecordService.insertBatch(records);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 查看料批详情
     * @author xin.huang
     * @date 2019/9/24
     * @return
     */
    @Override
    public ResponseResult<MaterialBatchDTO> findInfo(Long batchId) {
        EntityWrapper<ProMaterialBatch> wrapper = new EntityWrapper<>(new ProMaterialBatch()
                .setStatus(Status.TRUE.getKey())
                .setId(batchId));
        ProMaterialBatch materialBatch = this.selectOne(wrapper);
        if (ObjectUtils.isEmpty(materialBatch)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new MaterialBatchDTO());
        }
        MaterialBatchDTO materialBatchDTO = BeanUtils.copyProperties(materialBatch, MaterialBatchDTO.class);
        List<ProMaterialBatchDetail> materialBatchDetails = materialBatchDetailService
                .selectList(new EntityWrapper<>(new ProMaterialBatchDetail()
                        .setBatchId(materialBatch.getId()).setStatus(Status.TRUE.getKey())));
        materialBatchDTO.setMaterialBatchDetailS(BeanUtils.assemble(MaterialBatchDetailDTO.class, materialBatchDetails));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, materialBatchDTO);
    }

    /**
     * @descriptin 根据plc设备id查询料批详情
     * @author xin.huang
     * @param plcId
     * @param plcId
     * @date 2019/10/12
     * @return
     */
    @Override
    public ResponseResult<MaterialBatchDTO> findInfoByPlcId(Integer plcId) {
        EntityWrapper<ProMaterialBatch> wrapper = new EntityWrapper<>(new ProMaterialBatch()
                .setStatus(Status.TRUE.getKey())
                .setActive(Status.TRUE.getKey())
                .setPlcId(plcId));
        ProMaterialBatch materialBatch = this.selectOne(wrapper);
        if (ObjectUtils.isEmpty(materialBatch)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new MaterialBatchDTO());
        }
        MaterialBatchDTO materialBatchDTO = BeanUtils.copyProperties(materialBatch, MaterialBatchDTO.class);
        //获取配料实时数据统计
        List<ProIngredientRecord> proIngredientRecords = ingredientRecordService
        		.selectList(new EntityWrapper<>(new ProIngredientRecord()
                	.setBatchId(materialBatch.getId()).setStatus(Status.TRUE.getKey())));
        materialBatchDTO.setMaterialBatchDetailS(BeanUtils.assemble(MaterialBatchDetailDTO.class, proIngredientRecords));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, materialBatchDTO);
    }

    /**
     * @descriptin 查询料批列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/9/24
     * @return
     */
    @Override
    public ResponseResult<List<MaterialBatchDTO>> findList(AuthPlatformUserInfo userInfo,
                                                           MaterialQueryRQ rq,
                                                           Pagination pagination) {
        if (ObjectUtils.isEmpty(rq)) {
            rq = new MaterialQueryRQ();
        }
        rq.setCompanyId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        List<MaterialBatchDTO> materialBatchList = materialBatchMapper.findList(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, materialBatchList, PageUtils.transToPage(pagination));
    }

    /**
     * @descriptin 查询料批下拉列表
     * @author xin.huang
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    public ResponseResult<List<MaterialBatchDTO>> findComboBoxList(AuthPlatformUserInfo userInfo, Integer havePlc) {
        EntityWrapper<ProMaterialBatch> wrapper = new EntityWrapper<>(new ProMaterialBatch()
                .setActive(Status.TRUE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId()));
        if (havePlc.equals(Status.TRUE.getKey())) {
            wrapper.isNotNull("plc_id");
        } else {
            wrapper.isNull("plc_id");
        }
        List<ProMaterialBatch> proMaterialBatches = this.selectList(wrapper);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.assemble(MaterialBatchDTO.class, proMaterialBatches));
    }

    /**
     * @descriptin 编辑料批模拟
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateMaterialBatch(MaterialBatchRQ rq, AuthPlatformUserInfo userInfo) {
        ProMaterialBatch materialBatch = this.selectOne(new EntityWrapper<>(new ProMaterialBatch()
                .setId(rq.getId()).setStatus(Status.TRUE.getKey())));
        if (ObjectUtils.isEmpty(materialBatch)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        if (!ObjectUtils.isEmpty(rq)) {
            ProMaterialBatch proMaterialBatch = BeanUtils.copyProperties(rq, ProMaterialBatch.class);
            proMaterialBatch.setModifyTime(new Date()).setModifyId(userInfo.getId());
            if (!updateById(proMaterialBatch)) {
                log.error("更新料批基本信息失败 类：{} 方法：{}", "ProMaterialBatchServiceImpl", "updateMaterialBatch");
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
            //更新料批明细
            if (!ObjectUtils.isEmpty(rq.getMaterialBatchDetailList())) {
                materialBatchDetailService.update(new ProMaterialBatchDetail().setStatus(Status.FALSE.getKey()),
                        new EntityWrapper<>(new ProMaterialBatchDetail().setBatchId(rq.getId())));
                List<ProMaterialBatchDetail> materialBatchDetails = BeanUtils.assemble(ProMaterialBatchDetail.class,
                        rq.getMaterialBatchDetailList());
                materialBatchDetails.forEach(detail -> {
                    detail.setBatchId(rq.getId()).setCreateTime(new Date()).setModifyTime(new Date());
                });
                if (!materialBatchDetailService.insertBatch(materialBatchDetails)) {
                    log.error("更新料批明细信息失败 类：{} 方法：{}", "ProMaterialBatchServiceImpl", "updateMaterialBatch");
                    return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
                }
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
