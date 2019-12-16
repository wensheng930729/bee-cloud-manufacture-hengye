package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleBackAssayResultMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleCarrierTransportDetailMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleLogisticsBatchMapper;
import com.bee.platform.cloud.si.manufacture.dto.SaleBackAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleBackAssayResult;
import com.bee.platform.cloud.si.manufacture.entity.SaleCarrierTransportDetail;
import com.bee.platform.cloud.si.manufacture.entity.SaleLogisticsBatch;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.SaleBackAssayResultSaveRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleBackAssayResultService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleTransportSectionService;
import com.bee.platform.common.constants.enums.EnumLogistics;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.google.common.base.Objects;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 销售车次化验结果反馈表 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-27
 */
@Service
public class SaleBackAssayResultServiceImpl extends ServiceImpl<SaleBackAssayResultMapper, SaleBackAssayResult> implements SaleBackAssayResultService {

    @Autowired
    private SaleBackAssayResultService saleBackAssayResultService;
    @Autowired
    private SaleCarrierTransportDetailMapper saleCarrierTransportDetailMapper;
    @Autowired
    private SaleBackAssayResultMapper saleBackAssayResultMapper;
    @Autowired
    private SaleLogisticsBatchMapper saleLogisticsBatchMapper;
    @Autowired
    private SaleTransportSectionService saleTransportSectionService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveAssayResult(SaleBackAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo) {
        List<String> detailIds = rq.getCarrierTransportDetailIds();
        Integer assayResult = rq.getAssayResult();
        List<SampleAssayResultDTO> resultList = rq.getResultList();
        String batchId = null;
        BigDecimal arrivalVolume = BigDecimal.ZERO;
        for (String detailId : detailIds) {
            if (!CollectionUtils.isEmpty(resultList)) {
                List<SaleBackAssayResult> list = new ArrayList<>();
                for (SampleAssayResultDTO resultDTO : resultList) {
                    list.add(new SaleBackAssayResult()
                            .setCarrierTransportDetailId(detailId)
                            .setAssayItem(resultDTO.getAssayItem())
                            .setAssayValue(resultDTO.getAssayValue())
                            .setTestUnit(resultDTO.getTestUnit())
                            .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .setCreateId(userInfo.getId())
                            .setCreator(userInfo.getName())
                            .setCreateTime(new Date()));
                }
                list.forEach(a -> {
                    if (Objects.equal(a.getTestUnit(), 0)) {
                        a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
                    } else if (Objects.equal(a.getTestUnit(), 1)) {
                        a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
                    }
                });
                saleBackAssayResultService.insertBatch(list);
            }
            // 修改承运方运输详情表中化验结果信息
            SaleCarrierTransportDetail detail = saleCarrierTransportDetailMapper.selectOne(
                    new SaleCarrierTransportDetail().setCarrierTransportDetailId(detailId)
                            .setStatus(Status.TRUE.getKey()));
            batchId = detail.getBatchId();
            arrivalVolume = arrivalVolume.add(detail.getCargoWeight());
            detail.setAssayResult(assayResult);
            detail.setArrivalStatus(EnumLogistics.to_factory.YES.getKey());
            detail.setHandleType(EnumStorage.ProcessMode.confirm_storage.getKey());
            detail.setModifyId(userInfo.getId());
            detail.setModifier(userInfo.getName());
            detail.setModifyTime(new Date());
            saleCarrierTransportDetailMapper.updateById(detail);
        }
        //更新批次中的到货量
        if (!StringUtils.isEmpty(batchId)) {
            SaleLogisticsBatch logisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch().setBatchId(batchId));
            BigDecimal arrivalVolumes = logisticsBatch.getArrivalVolume() != null ? logisticsBatch.getArrivalVolume().add(arrivalVolume) : arrivalVolume;
            logisticsBatch.setArrivalVolume(arrivalVolumes);
            saleLogisticsBatchMapper.updateById(logisticsBatch);

            //更新合同的已收货量
            saleTransportSectionService.updateContractReceivedVolume(logisticsBatch.getContractBusinessId(), arrivalVolume, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<List<SaleBackAssayResultDTO>> getAssayResult(String carrierTransportDetailId) {
        List<SaleBackAssayResultDTO> list = new ArrayList<>();
        // 根据样品id查询分析项及结果（输出的）
        List<SaleBackAssayResult> assayResults = saleBackAssayResultMapper.selectList(
                new EntityWrapper<SaleBackAssayResult>()
                        .eq("carrier_transport_detail_id", carrierTransportDetailId)
                        .eq("status", Status.TRUE.getKey()));
        if (CollectionUtils.isEmpty(assayResults)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        }
        list = BeanUtils.assemble(SaleBackAssayResultDTO.class, assayResults);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

}
