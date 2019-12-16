package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleSampleMapper;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleSaleAddContractBusinessIdDTO;
import com.bee.platform.cloud.si.manufacture.dto.TonContractRelationDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.SampleAssayResultService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleSampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleSampleTonRelationService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.validation.constraints.NotBlank;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 销售取样表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-26
 */
@Slf4j
@Service
public class SaleSampleServiceImpl extends ServiceImpl<SaleSampleMapper, SaleSample> implements SaleSampleService {

    @Autowired
    private SaleSampleMapper sampleMapperSale;
    @Autowired
    private SaleSampleTonRelationService saleSampleTonRelationService;
    @Autowired
    private SaleContractBasicService saleContractBasicService;
    @Autowired
    private SampleService sampleService;
    @Autowired
    private BarCodeService barCodeService;
    @Autowired
    private SampleAssayResultService sampleAssayResultService;


    /**
     * 保存取样信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveSample(SaveSampleSaleRQ rq, AuthPlatformUserInfo userInfo) {
        BarCode barCode = barCodeService.checkCodeExist(rq.getSampleCode());
        if (barCode == null) {
            log.info("未找到对应的条形码，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
        } else if (Objects.equals(barCode.getUsed(), Status.TRUE.getKey())) {
            log.info("条形码已使用，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_ALREADY_USED);
        }
        // 根据合同编号查询合同
        SaleContractBasic contractBasic = saleContractBasicService.selectOne(new EntityWrapper<>(new SaleContractBasic()
                .setStatus(Status.TRUE.getKey())
                .setContractNum(rq.getContractNum())));
        if (contractBasic == null) {
            log.error("合同未找到，类：{}，contractNum：{}", "SaleSampleServiceImpl", rq.getContractNum());
            return ResponseResult.buildResponseResult(ResCodeEnum.CONTRACT_NOT_FOUND);
        }
        String sampleCode = rq.getSampleCode();
        // 插入样品相关
        SaleSample sample = BeanUtils.copyProperties(rq, SaleSample.class)
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setAssayStatus(EnumSampleRelation.SampleAssayStatus.PREPARE_ASSAY.getKey())
                .setStatus(1)
                .setCreateId(userInfo.getId())
                .setCreator(userInfo.getName())
                .setCreateTime(new Date())
                .setContractBusinessId(contractBasic.getContractBusinessId());
        sampleMapperSale.insert(sample);
        // 插入样品与吨袋相关
        List<String> tonCodeList = rq.getTonCodeList();
        ArrayList<SaleSampleTonRelation> list = new ArrayList<>(tonCodeList.size());
        for (String s : tonCodeList) {
            list.add(new SaleSampleTonRelation().setSampleCode(sampleCode)
                    .setTonCode(s)
                    .setStatus(1)
                    .setCreateId(userInfo.getId())
                    .setCreator(userInfo.getName())
                    .setCreateTime(new Date()));
        }
        saleSampleTonRelationService.insertBatch(list);
        // 取样的时候合同肯定存在：不需要修改对应合同的推送状态
        // 更新条形码编号为已使用
        barCodeService.updateCodeUsed(barCode);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 完成取样
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleSaleRQ rq, AuthPlatformUserInfo userInfo) {
        String contractBusinessId = rq.getContractBusinessId();
        SaleContractBasic contractBasic = saleContractBasicService.selectOne(new EntityWrapper<>(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(1)));
        if (contractBasic == null) {
            log.error("销售完成取样未找到对应的合同 类{}", "SaleSampleServiceImpl");
            return ResponseResult.buildResponseResult(ResCodeEnum.CONTRACT_NOT_FOUND);
        }
        // 更新合同为已取样
        saleContractBasicService.updateById(contractBasic
                .setSampleStatus(EnumSampleRelation.SampleStatus.COMPLETED.getKey())
                .setModifier(userInfo.getName())
                .setModifyId(userInfo.getId())
                .setModifyTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 化验人员开始化验样品
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo) {
        @NotBlank String sampleCode = rq.getSampleCode();
        if (o == null) {
            log.info("未找到销售样品--样品编号SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        SaleSample sample = (SaleSample) o;
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())
                .setModifier(userInfo.getName())
                .setModifyId(userInfo.getId())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存样品化验结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo) {
        String sampleCode = rq.getSampleCode();
        Integer productId = rq.getProductId();
        List<SampleAssayResultDTO> resultList = rq.getResultList();
        // 查询对应样品
        SaleSample sample = this.selectOne(new EntityWrapper<>(new SaleSample()
                .setSampleCode(sampleCode)
                .setEnterpriseId(userInfo.getOrgId())
                .setStatus(1)));
        if (sample == null) {
            log.info("未查询到该样品，方法：{}sampleCode:{} ", "saveAssayResult", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        // 多次保存 将以前保存的化验结果逻辑删除
        sampleAssayResultService.update(new SampleAssayResult().setStatus(0),
                new EntityWrapper<>(new SampleAssayResult()
                        .setSampleCode(rq.getSampleCode())
                        .setStatus(1)));
        // 查询样品输出项,并计算保存输出项结果
        sampleService.computeAndsaveAssayResult(sampleCode, productId, resultList,
                EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 化验人员弃用样品
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo) {
        SaleSample sample = this.selectOne(new EntityWrapper<>(new SaleSample()
                .setSampleCode(rq.getSampleCode())
                .setStatus(1)));
        if (sample == null) {
            log.info("未查询到该样品，类：{}，方法：{},sampleCode:{} ", "SaleSampleServiceImpl", "saveAssayResult", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .setAbandonReason(rq.getAbandonReason())
                .setAbandonPerson(userInfo.getName())
                .setAbandonId(userInfo.getId())
                .setAbandonTime(new Date())
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据样品code获取样品详情
     *
     * @param o
     * @param sampleCode
     * @return
     */
    @Override
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode) {
        SaleSample sample = (SaleSample) o;
        if (!Objects.equals(sample.getAssayStatus(), EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())) {
            log.info("改条码的样品不在销售化验中样品的列表，样品code：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_IN_PREPARE_ASSAY);
        }
        SampleAssayDetailDTO dto = BeanUtils.copyProperties(sample, SampleAssayDetailDTO.class);
        dto.setBusinessType(EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @Deprecated
    public boolean sampleSaleSetContractBusinessId(List<String> tonCodeList) {
        // 根据吨袋编号 查询 和出库绑定的销售合同业务id
        List<TonContractRelationDTO> dtoList = sampleMapperSale.getTonCodeContractBusinessIdRelation(tonCodeList);
        // 根据吨袋编号 查询 和吨袋绑定的样品
        List<SampleSaleAddContractBusinessIdDTO> businessIdDTOList = sampleMapperSale.getSampleSaleByTonCode(tonCodeList);
        List<SaleSample> sampleList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(dtoList) && !CollectionUtils.isEmpty(businessIdDTOList)) {
            for (SampleSaleAddContractBusinessIdDTO idDTO : businessIdDTOList) {
                for (TonContractRelationDTO dto : dtoList) {
                    // 如果样品的合同业务id为空
                    if (StringUtils.isBlank(idDTO.getContractBusinessId())
                            && Objects.equals(idDTO.getTonCode(), dto.getTonCode())) {
                        sampleList.add(new SaleSample()
                                .setId(idDTO.getId())
                                .setContractBusinessId(dto.getContractBusinessId()));
                        break;
                    }
                }
            }
        }
        if (sampleList.size() > 0) {
            this.updateBatchById(sampleList);
            return true;
        }
        return false;
    }
}
