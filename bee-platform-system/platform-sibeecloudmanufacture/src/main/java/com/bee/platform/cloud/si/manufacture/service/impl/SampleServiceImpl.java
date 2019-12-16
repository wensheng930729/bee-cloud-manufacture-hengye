package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.cloud.si.manufacture.service.SampleAssayResultTemporaryService;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleWeightRelationService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.SampleAssayResultService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProBaggingService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProSampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleSampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleSampleTonRelationService;
import com.bee.platform.common.dao.mapper.SequenceMapper;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.IntegerUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.PlaceholderUtils;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.validation.constraints.NotBlank;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author liang.li
 * @ClassName SampleServiceImpl
 * @Description 样品公共方法
 * @Date 2019-9-30
 */
@Slf4j
@Service
public class SampleServiceImpl implements SampleService {
    @Autowired
    private SaleSampleMapper sampleMapperSale;
    @Autowired
    private SaleSampleTonRelationService saleSampleTonRelationService;
    @Autowired
    private BuySampleService sampleServiceBuy;
    @Autowired
    private SaleSampleService sampleServiceSale;
    @Autowired
    private ProSampleService sampleServicePro;
    @Autowired
    private BuySampleWeightRelationService buySampleWeightRelationService;
    @Autowired
    private BuySampleMapper buySampleMapper;
    @Autowired
    private SampleAssayResultMapper sampleAssayResultMapper;
    @Autowired
    private SampleAssayResultService sampleAssayResultService;
    @Autowired
    private ConfigProductService productService;
    @Autowired
    private BarCodeService barCodeService;
    @Autowired
    private SequenceMapper sequenceMapper;
    @Autowired
    private ProBaggingService proBaggingService;
    @Autowired
    private ProSampleService proSampleService;
    @Autowired
    private StorageService storageService;
    @Autowired
    private BuySampleWeightRelationMapper buySampleWeightRelationMapper;
    @Autowired
    private FinishedProductBeOutOfStorageMapper finishedProductBeOutOfStorageMapper;
    @Autowired
    private SampleAssayResultTemporaryService sampleAssayResultTemporaryService;

    @Value("${code.random}")
    private String codeRandom;
    @Value("${code.codeLength}")
    private String codeLength;

    @Override
    public ResponseResult<List<SamplePrepareDTO>> getSamplePrepareList(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Map<String, Object> map = new HashMap<>(3);
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("sampleStatus", EnumSampleRelation.SampleStatus.NOT_YET.getValue());
        // 查询到所有业务线待取样数据
        List<SamplePrepareDTO> prepareList = sampleMapperSale.getSamplePrepareList(pagination, map);
        if (CollectionUtils.isEmpty(prepareList)) {
            log.info("未查询到未取样列表信息 类：{}，方法：{}", "SaleSampleServiceImpl", "getSamplePrepareList");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0),
                    PageUtils.transToPage(pagination));
        }
        List<SamplePrepareDTO> resultList = new ArrayList<>();
        // 采购列表
        List<SamplePrepareDTO> buyList = prepareList.stream().filter(
                a -> a.getBusinessType().equals(EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(buyList)) {
            // 查询样品code
            getPrepareBuySampleCode(buyList);
            // 组装结果list
            resultList.addAll(buyList);
        }
        // 销售列表
        List<SamplePrepareDTO> saleList = prepareList.stream().filter(
                a -> a.getBusinessType().equals(EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(saleList)) {
            // 查询样品code
            getPrepareSaleSampleCode(saleList);
            // 组装结果list
            resultList.addAll(saleList);
        }
        // 生产列表
        List<SamplePrepareDTO> proList = prepareList.stream().filter(
                a -> a.getBusinessType().equals(EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(proList)) {
            // 查询样品code
            getPrepareProSampleCode(proList);
            // 组装结果list
            resultList.addAll(proList);
        }
        resultList.forEach(a -> {
            // 类型根据EnumSampleRelation的SampleAssayResultBusinessType确定
            switch (a.getBusinessType()) {
                case 1:
                    a.setBusinessTypeName("采购取样");
                    break;
                case 2:
                    a.setBusinessTypeName("销售取样");
                    break;
                case 3:
                    a.setBusinessTypeName("生产取样");
                    break;
                default:
                    a.setBusinessTypeName("");
                    break;
            }
        });
        resultList.sort((o1, o2) -> {
            Date o1SamplePushTime = o1.getSamplePushTime();
            Date o2SamplePushTime = o2.getSamplePushTime();
            if (o1SamplePushTime != null && o2SamplePushTime != null) {
                return o2SamplePushTime.compareTo(o1SamplePushTime);
            }
            return 0;
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询生产样品code
     */
    private void getPrepareProSampleCode(List<SamplePrepareDTO> proList) {
        Set<Integer> idSet = proList.stream().map(a -> a.getProOreFurnaceSampleId()).collect(Collectors.toSet());
        List<ProSample> sampleList = sampleServicePro.selectList(new EntityWrapper<ProSample>()
                .eq("status", 1)
                .in("pro_ore_furnace_sample_id", idSet));
//        DateFormat df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");
        for (SamplePrepareDTO dto : proList) {
//            if (dto.getBakedCompleteTime() != null) {
//                String formatTime = df.format(dto.getBakedCompleteTime());
//                dto.setBakedDate(formatTime.substring(0, 11));
//                dto.setBakedTime(formatTime.substring(12).trim());
//            }
            List<String> list = Lists.newArrayList();
            for (ProSample sample : sampleList) {
                if (Objects.equal(dto.getProOreFurnaceSampleId(), sample.getProOreFurnaceSampleId())) {
                    list.add(sample.getSampleCode());
                }
            }
            dto.setSampleCodeList(list);
        }
    }

    /**
     * 查询销售样品code
     */
    private void getPrepareSaleSampleCode(List<SamplePrepareDTO> saleList) {
        Set<String> stringSet = saleList.stream().map(SamplePrepareDTO::getContractBusinessId).collect(Collectors.toSet());
        List<SaleSample> sampleList = sampleMapperSale.selectList(new EntityWrapper<SaleSample>()
                .eq("status", 1)
                .in("contract_business_id", stringSet));
        for (SamplePrepareDTO dto : saleList) {
            List<String> list = Lists.newArrayList();
            for (SaleSample saleSample : sampleList) {
                if (dto.getContractBusinessId().equals(saleSample.getContractBusinessId())) {
                    list.add(saleSample.getSampleCode());
                }
            }
            dto.setSampleCodeList(list);
        }
    }

    /**
     * 查询采购样品code
     */
    private void getPrepareBuySampleCode(List<SamplePrepareDTO> buyList) {
        Set<String> stringSet = buyList.stream().map(SamplePrepareDTO::getMachineId).collect(Collectors.toSet());
        List<BuySampleWeightRelation> buySampleWeightRelations = buySampleWeightRelationService.selectList(new EntityWrapper<BuySampleWeightRelation>()
                .eq("status", 1)
                .in("machine_id", stringSet));
        DateFormat df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");
        for (SamplePrepareDTO dto : buyList) {
            List<String> list = Lists.newArrayList();
            if (dto.getWeightCompleteTime() != null) {
                String formatTime = df.format(dto.getWeightCompleteTime());
                dto.setWeightDate(formatTime.substring(0, 11));
                dto.setWeightTime(formatTime.substring(12).trim());
            }
            for (BuySampleWeightRelation relation : buySampleWeightRelations) {
                if (dto.getMachineId().equals(relation.getMachineId())) {
                    list.add(relation.getSampleCode());
                }
            }
            dto.setSampleCodeList(list);
        }
    }

    @Override
    public ResponseResult<List<SampleAlreadyDTO>> getSampleAlreadyList(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Map<String, Object> map = new HashMap<>(2);
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        List<SampleAlreadyDTO> alreadyList = sampleMapperSale.getSampleAlreadyList(pagination, map);
        if (CollectionUtils.isEmpty(alreadyList)) {
            log.info("未查询到已取样列表信息 类：{}，方法：{}", "SaleSampleServiceImpl", "getSampleAlreadyList");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0),
                    PageUtils.transToPage(pagination));
        }
        // 销售的列表--需要添加吨袋信息
        List<SampleAlreadyDTO> saleList = alreadyList.stream().filter(
                a -> a.getBusinessType().equals(EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())).collect(Collectors.toList());
        List<SampleAlreadyDTO> resultList;
        if (!CollectionUtils.isEmpty(saleList)) {
            // 查询已取样列表销售的吨袋信息
            resultList = getAlreadySaleTonCode(alreadyList, saleList);
        } else {
            resultList = alreadyList;
        }
        resultList.forEach(a -> {
            // 类型根据EnumSampleRelation的SampleAssayResultBusinessType确定
            switch (a.getBusinessType()) {
                case 1:
                    a.setBusinessTypeName("采购取样");
                    break;
                case 2:
                    a.setBusinessTypeName("销售取样");
                    break;
                case 3:
                    a.setBusinessTypeName("生产取样");
                    break;
                default:
                    a.setBusinessTypeName("");
                    break;
            }
        });
        resultList.sort((o1, o2) -> {
            Date o1SampleTime = o1.getSampleTime();
            Date o2SampleTime = o2.getSampleTime();
            if (o1SampleTime != null && o2SampleTime != null) {
                return o2SampleTime.compareTo(o1SampleTime);
            }
            return 0;
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询已取样列表销售的吨袋信息
     */
    private List<SampleAlreadyDTO> getAlreadySaleTonCode(List<SampleAlreadyDTO> alreadyList, List<SampleAlreadyDTO> saleList) {
        // 设置样品和吨袋关联关系
        Set<String> stringSet = saleList.stream().map(SampleAlreadyDTO::getSampleCode).collect(Collectors.toSet());
        List<SaleSampleTonRelation> tonRelations = saleSampleTonRelationService.selectList(new EntityWrapper<SaleSampleTonRelation>()
                .eq("status", 1)
                .in("sample_code", stringSet));
        for (SampleAlreadyDTO dto : saleList) {
            List<String> list = new ArrayList<>();
            for (SaleSampleTonRelation tonRelation : tonRelations) {
                if (dto.getSampleCode().equals(tonRelation.getSampleCode())) {
                    list.add(tonRelation.getTonCode());
                }
            }
            dto.setTonCodeList(list);
        }
        // 将销售的list删除，添加吨袋信息之后再重新添加
        List<SampleAlreadyDTO> resultList = alreadyList.stream().filter(
                a -> !Objects.equal(a.getBusinessType(), EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())).collect(Collectors.toList());
        resultList.addAll(saleList);
        return resultList;
    }

    @Override
    public ResponseResult getAssayList(AuthPlatformUserInfo userInfo, Integer assayStatus, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        if (EnumSampleRelation.SampleAssayStatus.PREPARE_ASSAY.getKey().equals(assayStatus)) {
            // 待化验
            return getAssayingAndPrepareList(assayStatus, userInfo, pagination);
        } else if (EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey().equals(assayStatus)) {
            // 化验中
            return getAssayingAndPrepareList(assayStatus, userInfo, pagination);
        } else if (EnumSampleRelation.SampleAssayStatus.ABANDON.getKey().equals(assayStatus)) {
            // 已弃用
            return getAssayAbandonList(assayStatus, userInfo, pagination);
        } else {
            // 已化验
            return getAssayAlreadyList(assayStatus, userInfo, pagination);
        }
    }

    /**
     * 查询化验人员已化验列表
     */
    private ResponseResult getAssayAlreadyList(Integer assayStatus, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Map<String, Object> map = new HashMap<>(3);
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("assayStatus", assayStatus);
        map.put("factoryId", userInfo.getFactoryId());
        List<SampleAssayAlreadyDTO> sampleList = buySampleMapper.getAssayAlreadyList(pagination, map);
        if (CollectionUtils.isEmpty(sampleList)) {
            log.info("未查询到已化验的样品,方法：{} assayStatus：", "BuySampleServiceImpl", assayStatus);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0)
                    , PageUtils.transToPage(pagination));
        }
        // 查询化验结果
        Set<String> sampleCodeList = sampleList.stream().map(SampleAssayAlreadyDTO::getSampleCode).collect(Collectors.toSet());
        List<SampleAssayResult> assayResultList = sampleAssayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                .eq("status", 1)
                .in("sample_code", sampleCodeList));
        for (SampleAssayAlreadyDTO dto : sampleList) {
            List<SampleAssayResultDTO> assayResultList1 = dto.getAssayResultList();
            if (assayResultList1 == null) {
                assayResultList1 = Lists.newArrayList();
            }
            // 遍历设置样品的输出项结果
            for (SampleAssayResult result : assayResultList) {
                if (java.util.Objects.equals(dto.getSampleCode(), result.getSampleCode())
                        && java.util.Objects.equals(result.getType(), EnumSampleRelation.SampleAssayResultType.OUT.getKey())) {
                    SampleAssayResultDTO e = BeanUtils.copyProperties(result, SampleAssayResultDTO.class);
                    if (e.getTestUnit() != null && e.getTestUnit().equals(0)) {
                        e.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
                    } else if (e.getTestUnit() != null && e.getTestUnit().equals(1)) {
                        e.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
                    }
                    assayResultList1.add(e);
                }
            }
            dto.setAssayResultList(assayResultList1);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, sampleList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询化验人员已弃用列表
     */
    private ResponseResult getAssayAbandonList(Integer assayStatus, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Map<String, Object> map = new HashMap<>(3);
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("assayStatus", assayStatus);
        map.put("factoryId", userInfo.getFactoryId());
        List<SampleAssayAbandonDTO> list = buySampleMapper.getAssayAbandonList(pagination, map);
        if (CollectionUtils.isEmpty(list)) {
            log.info("未查询到已弃用的样品,方法：{} assayStatus：", "BuySampleServiceImpl", assayStatus.toString());
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0)
                    , PageUtils.transToPage(pagination));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

    /**
     * 查询化验中和待化验列表
     */
    private ResponseResult getAssayingAndPrepareList(Integer assayStatus, AuthPlatformUserInfo userInfo, Pagination pagination) {
        Map<String, Object> map = new HashMap<>(3);
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("assayStatus", assayStatus);
        map.put("factoryId", userInfo.getFactoryId());
        List<SampleAssayPrepareAndAssayingDTO> list = buySampleMapper.getAssayingAndPrepareList(pagination, map);
        if (CollectionUtils.isEmpty(list)) {
            log.info("未查询到化验中或待化验的样品,方法：{} assayStatus：", "BuySampleServiceImpl", assayStatus);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0)
                    , PageUtils.transToPage(pagination));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<ResCodeEnum> abandonSample(AuthPlatformUserInfo userInfo, SampleAssayAbandonRQ rq) {
        Integer businessType = rq.getBusinessType();
        if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey())) {
            return sampleServiceBuy.abandonSample(rq, userInfo);
        } else if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())) {
            return sampleServiceSale.abandonSample(rq, userInfo);
        } else if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())) {
            return sampleServicePro.abandonSample(rq, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNKNOWN_BUSINESS);
        }
    }

    @Override
    public ResponseResult<ResCodeEnum> startAssaySample(AuthPlatformUserInfo userInfo, SampleAssayStartRQ rq) {
        @NotBlank String sampleCode = rq.getSampleCode();
        Object sample = getSampleEntityBySampleCode(sampleCode);
        if (sample instanceof BuySample) {
            return sampleServiceBuy.startAssaySample(rq, sample, userInfo);
        } else if (sample instanceof SaleSample) {
            return sampleServiceSale.startAssaySample(rq, sample, userInfo);
        } else if (sample instanceof ProSample) {
            return sampleServicePro.startAssaySample(rq, sample, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
    }

    @Override
    public ResponseResult<ResCodeEnum> saveAssayResult(AuthPlatformUserInfo userInfo, SampleAssayResultSaveRQ rq) {
        Integer businessType = rq.getBusinessType();
        if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey())) {
            return sampleServiceBuy.saveAssayResult(rq, userInfo);
        } else if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())) {
            return sampleServiceSale.saveAssayResult(rq, userInfo);
        } else if (Objects.equal(businessType, EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())) {
            return sampleServicePro.saveAssayResult(rq, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNKNOWN_BUSINESS);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveTemporaryAssayResult(AuthPlatformUserInfo userInfo, SampleAssayResultSaveRQ rq) {
        List<SampleAssayResultDTO> list = rq.getResultList();
        if (CollectionUtils.isEmpty(list)) {
            log.info("保存临时化验结果----输入项为空：{}", list);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        List<SampleAssayResultTemporary> temporaryList = BeanUtils.assemble(SampleAssayResultTemporary.class, list);
        temporaryList.forEach(a -> {
            a.setCreateId(userInfo.getId())
                    .setCreator(userInfo.getName())
                    .setCreateTime(new Date())
                    .setBusinessType(rq.getBusinessType())
                    .setSampleCode(rq.getSampleCode())
                    .setStatus(Status.TRUE.getKey());
            if (Objects.equal(a.getTestUnit(), 0)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
            } else if (Objects.equal(a.getTestUnit(), 1)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
            }
        });
        // 删除上次暂存数据
        sampleAssayResultTemporaryService.update(new SampleAssayResultTemporary().setStatus(0),
                new EntityWrapper<>(new SampleAssayResultTemporary().setSampleCode(rq.getSampleCode())));
        // 插入新的暂存数据
        sampleAssayResultTemporaryService.insertBatch(temporaryList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<List<SampleAssayResultDTO>> getTemporaryAssayResult(String sampleCode) {
        List<SampleAssayResultTemporary> temporaryList = sampleAssayResultTemporaryService.selectList(new EntityWrapper<>(new SampleAssayResultTemporary()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sampleCode)));
        if (CollectionUtils.isEmpty(temporaryList)) {
            log.info("临时化验结果为空,sampelCode:{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>());
        }
        List<SampleAssayResultDTO> resultList = BeanUtils.assemble(SampleAssayResultDTO.class, temporaryList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList);
    }

    @Override
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(AuthPlatformUserInfo userInfo, String sampleCode) {
        Object o = getSampleEntityBySampleCode(sampleCode);
        if (o == null) {
            log.info("未找到样品--样品编号SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        if (o instanceof BuySample) {
            return sampleServiceBuy.getSampleAssayDetailByCode(o, sampleCode);
        } else if (o instanceof SaleSample) {
            return sampleServiceSale.getSampleAssayDetailByCode(o, sampleCode);
        } else if (o instanceof ProSample) {
            return sampleServicePro.getSampleAssayDetailByCode(o, sampleCode);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<List<String>> generateSampleCode(SampleCodeRQ rq) {
        Integer codeType = rq.getCodeType();
        Integer length = rq.getLength();
        // 总长度
        int codeLengthInt = Integer.valueOf(codeLength);
        // 中间随机数长度
        int codeRandomLength = codeRandom.length();
        // 自增序列补充后的总长度
        int seqLength = codeLengthInt - codeRandomLength - 1;

        if (length < 1) {
            log.error("生成编码长度错误！长度：{}", length);
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        // 查询开始序列
        Sequence barCodeSeq = sequenceMapper.selectOne(new Sequence().setSequenceKey("barCodeSeq").setStatus(1));
        Integer sequenceValue = barCodeSeq.getSequenceValue();

        ArrayList<BarCode> list = new ArrayList<>(length);
        StringBuilder sb = new StringBuilder();

        sb.append(codeType);
        sb.append(codeRandom);

        for (int i = sequenceValue; i < sequenceValue + length; i++) {
            sb.append(IntegerUtils.getDecorateInteger(seqLength, i));
            list.add(new BarCode()
                    .setType(codeType)
                    .setCode(sb.toString())
                    .setDate(LocalDate.now().toString())
                    .setUsed(0)
                    .setStatus(1));
            sb.delete(1 + codeRandomLength, sb.length());
        }
        // 插入数据库
        barCodeService.insertBatch(list);
        // 更新序列
        sequenceMapper.update(new Sequence().setSequenceValue(sequenceValue + length),
                new EntityWrapper<>(new Sequence().setSequenceKey("barCodeSeq").setStatus(1)));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public void computeAndsaveAssayResult(String sampleCode, Integer productId, List<SampleAssayResultDTO> resultList, Integer businessType, AuthPlatformUserInfo userInfo) {
        resultList.forEach(a -> a.setMarkIn(a.getMarkIn().substring(2, a.getMarkIn().length() - 1)));
        Map<String, Double> inMap = resultList.stream().collect(
                Collectors.toMap(SampleAssayResultDTO::getMarkIn, SampleAssayResultDTO::getAssayValue, (oldValue, newValue) -> newValue));
        List<ConfigProductTestAttributeOutDTO> attributeOutList = productService.getProductAttributeOutByProductId(productId, userInfo);
        if (CollectionUtils.isEmpty(attributeOutList)) {
            log.info("未查询到产品输出项信息，产品id：{}", productId);
            return;
        }
        // 最后保存的list
        List<SampleAssayResult> list = new ArrayList<>();
        // 保存输入项和输出项item和单位的map
        Map<String, Integer> itemUnitRelationMap = new HashMap<>();
        for (SampleAssayResultDTO resultDTO : resultList) {
            list.add(new SampleAssayResult()
                    .setSampleCode(sampleCode)
                    .setAssayItem(resultDTO.getAssayItem())
                    .setAssayValue(resultDTO.getAssayValue())
                    .setTestUnit(resultDTO.getTestUnit())
                    .setType(EnumSampleRelation.SampleAssayResultType.IN.getKey())
                    .setBusinessType(businessType)
                    .setStatus(1)
                    .setCreateId(userInfo.getId())
                    .setCreator(userInfo.getName())
                    .setCreateTime(new Date()));
            // 将输入项的item标识和单位放入map
            itemUnitRelationMap.put(resultDTO.getMarkIn(), resultDTO.getTestUnit());
        }
        // 保留计算结果的数据
        NumberFormat nf = NumberFormat.getNumberInstance();
        nf.setRoundingMode(RoundingMode.HALF_UP);

        for (ConfigProductTestAttributeOutDTO outDTO : attributeOutList) {
            // 保留位数
            nf.setMaximumFractionDigits(outDTO.getDecimalDigit() == null ? 3 : outDTO.getDecimalDigit());
            // 计算公式
            String assayFormula = outDTO.getAssayFormula();
            // 统一输入项和输出项的单位
            Map<String, Double> inMapCopy = unifyUnit(itemUnitRelationMap, new HashMap<>(inMap), outDTO.getTestUnit());
            // 计算结果
            Double assayValue = computeAssayResultOutValue(inMapCopy, assayFormula);
            assayValue = Double.valueOf(nf.format(assayValue).replace(",", ""));
            // 输出项item
            String assayItemOut = outDTO.getAssayItemOut();
            // 输出项的公式标识
            String markOut = outDTO.getMarkOut();
            // 将结算结果放入map，以便下面的公式引用上面的结果
            String itemRepresent = markOut.substring(2, markOut.length() - 1);
            inMap.put(itemRepresent, assayValue);
            // 将已经计算过的输出项的：item标识和单位放入单位map
            itemUnitRelationMap.put(itemRepresent, outDTO.getTestUnit());

            list.add(new SampleAssayResult()
                    .setSampleCode(sampleCode)
                    .setAssayItem(assayItemOut)
                    .setAssayValue(assayValue)
                    .setTestUnit(outDTO.getTestUnit())
                    .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                    .setBusinessType(businessType)
                    .setStatus(1)
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
        sampleAssayResultService.insertBatch(list);
    }

    /**
     * 保证输入项与输出项的单位统一
     *
     * @param itemUnitRelationMap 保存输入项和已经计算过的输出项
     * @param inMap               保存输入项和已经计算过的输出项 与结果
     * @param outUnit             输出项化验单位（0 %百分比  1 ‱万分比）
     */
    private Map<String, Double> unifyUnit(Map<String, Integer> itemUnitRelationMap, Map<String, Double> inMap, Integer outUnit) {
        Set<String> keySet = itemUnitRelationMap.keySet();
        // 输出项是百分之  如果输入项不同则是万分之 需要把结果扩大100倍
        if (EnumSampleRelation.ProductUnit.PERCENTAGE.getKey().equals(outUnit)) {
            for (String key : keySet) {
                if (!outUnit.equals(itemUnitRelationMap.get(key))) {
                    inMap.put(key, inMap.get(key) / 100);
                }
            }
            return inMap;
        } else if (EnumSampleRelation.ProductUnit.EXTREME_RATIO.getKey().equals(outUnit)) {
            // 输出项是万分之  如果输入项不同则是百分之 需要把结果缩小100倍
            for (String key : keySet) {
                if (!outUnit.equals(itemUnitRelationMap.get(key))) {
                    inMap.put(key, inMap.get(key) * 100);
                }
            }
            return inMap;
        } else {
            log.info("计算化验结果项结果失败，输出项未知单位标识：{}", outUnit);
            return inMap;
        }
    }

    @Override
    public Double computeAssayResultOutValue(Map<String, Double> inMap, String assayFormula) {
        HashMap<String, Object> inMapCopy = new HashMap<>(inMap);
        String s = null;
        try {
            s = PlaceholderUtils.resolvePlaceholders(assayFormula, inMapCopy);
            ScriptEngineManager manager = new ScriptEngineManager();
            ScriptEngine se = manager.getEngineByName("js");
            return Double.valueOf(se.eval(s).toString());
        } catch (ScriptException e) {
            log.error("根据公式计算化验结果输出项出错啦！配置公式：{},计算公式：{}，数据：{}", assayFormula, s, inMap);
            e.printStackTrace();
            throw new BusinessException(ResCodeEnum.FORMULA_ERROR, ExceptionMessageEnum.FORMULA_ERROR);
        }
    }

    @Override
    public Object getSampleEntityBySampleCode(String sampleCode) {
        BuySample buySample = sampleServiceBuy.selectOne(new EntityWrapper<>(new BuySample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (buySample != null) {
            return buySample;
        }
        SaleSample saleSample = sampleServiceSale.selectOne(new EntityWrapper<>(new SaleSample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (saleSample != null) {
            return saleSample;
        }
        ProSample proSample = sampleServicePro.selectOne(new EntityWrapper<>(new ProSample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (proSample != null) {
            return proSample;
        }
        ProBagging proBagging = proBaggingService.selectOne(new EntityWrapper<>(new ProBagging()
                .setBaggingCode(sampleCode)
                .setStatus(1)));
        if (proBagging != null) {
            return proBagging;
        }
        return null;
    }

    @Override
    public boolean checkSampleCodeUsed(String sampleCode) {
        int a = sampleServiceBuy.selectCount(new EntityWrapper<>(new BuySample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (a > 0) {
            return true;
        }
        a = sampleServiceSale.selectCount(new EntityWrapper<>(new SaleSample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (a > 0) {
            return true;
        }
        a = sampleServicePro.selectCount(new EntityWrapper<>(new ProSample()
                .setSampleCode(sampleCode)
                .setStatus(1)));
        if (a > 0) {
            return true;
        }
        return false;
    }

    @Override
    public ResponseResult<List<SampleAssayResultOutDTO>> getSampleAssayResultOut(AuthPlatformUserInfo userInfo, String sampleCode) {
        List<SampleAssayResult> resultList = sampleAssayResultService.selectList(new EntityWrapper<>(new SampleAssayResult()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sampleCode)
                .setType(Status.TRUE.getKey())));
        if (CollectionUtils.isEmpty(resultList)) {
            log.info("没有查询到化验结果输出项，样品code：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<SampleAssayResultOutDTO> dtoList = BeanUtils.assemble(SampleAssayResultOutDTO.class, resultList);
        dtoList.forEach(a -> {
            if (a.getTestUnit() != null && a.getTestUnit().equals(0)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
            } else if (a.getTestUnit() != null && a.getTestUnit().equals(1)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
            }
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<ResCodeEnum> saveSampleProductSpec(AuthPlatformUserInfo userInfo, SampleSaveProductSpecRQ rq) {
        String sampleCode = rq.getSampleCode();
        Object o = getSampleEntityBySampleCode(sampleCode);
        if (o == null) {
            log.info("对应样品未找到，code：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        // 保存化验结果后的规格 并将状态修改为已化验
        if (o instanceof BuySample) {
            BuySample sample = (BuySample) o;
            sample.setProductSpecId(rq.getProductSpecId())
                    .setProductSpecName(rq.getProductSpecName())
                    .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                    .setAssayPerson(userInfo.getName())
                    .setAssayId(userInfo.getId())
                    .setAssayTime(new Date())
                    .setModifyId(userInfo.getId())
                    .setModifier(userInfo.getName())
                    .setModifyTime(new Date());
            sampleServiceBuy.updateById(sample);
        } else if (o instanceof SaleSample) {
            SaleSample sample = (SaleSample) o;
            sample.setProductSpecId(rq.getProductSpecId())
                    .setProductSpecName(rq.getProductSpecName())
                    .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                    .setAssayPerson(userInfo.getName())
                    .setAssayId(userInfo.getId())
                    .setAssayTime(new Date())
                    .setModifyId(userInfo.getId())
                    .setModifier(userInfo.getName())
                    .setModifyTime(new Date());
            sampleServiceSale.updateById(sample);
        } else if (o instanceof ProSample) {
            ProSample sample = (ProSample) o;
            sample.setProductSpecId(rq.getProductSpecId())
                    .setProductSpecName(rq.getProductSpecName())
                    .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                    .setAssayPerson(userInfo.getName())
                    .setAssayId(userInfo.getId())
                    .setAssayTime(new Date())
                    .setModifyId(userInfo.getId())
                    .setModifier(userInfo.getName())
                    .setModifyTime(new Date());
            sampleServicePro.updateById(sample);
            // 没有规格的入库记录还未入库--设置产品规格并把数量统计入仓库
            updateBagSpecAndStorage(rq, sampleCode, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNKNOWN_BUSINESS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    private void updateBagSpecAndStorage(SampleSaveProductSpecRQ rq, String sampleCode, AuthPlatformUserInfo userInfo) {
        // 该code下的装袋记录
        List<ProBagging> bagList = proBaggingService.selectList(new EntityWrapper<>(new ProBagging()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sampleCode)));
        if (!CollectionUtils.isEmpty(bagList)) {
            // 更新没有规格的装袋记录
            List<ProBagging> noSpecBagList = bagList.stream().filter(a -> a.getProductSpecId() == null).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(noSpecBagList)) {
                // 更新没有规格的装袋记录
                noSpecBagList.stream().forEach(a -> {
                    a.setProductSpecId(rq.getProductSpecId())
                            .setProductSpecName(rq.getProductSpecName());
                });
                proBaggingService.updateBatchById(noSpecBagList);
            }

            // 更新入库记录的规格
            List<ProBagStorageSpecUpdateDTO> list = Lists.newArrayList();
            bagList.forEach(a -> {
                list.add(new ProBagStorageSpecUpdateDTO()
                        .setBaggingCode(a.getBaggingCode())
                        .setProductSpecId(rq.getProductSpecId())
                        .setProductSpecName(rq.getProductSpecName()));
            });
            storageService.updateFinishedProductSpecAndInsertStorage(list, userInfo);
        }
    }

    @Override
    public ResponseResult getDetailByCode(String sampleCode) {
        Object entity = this.getSampleEntityBySampleCode(sampleCode);
        if (entity == null) {
            BarCode barCode = barCodeService.selectOne(new EntityWrapper<>(new BarCode()
                    .setStatus(Status.TRUE.getKey())
                    .setCode(sampleCode)));
            if (barCode == null) {
                log.info("数据库未找到该code-Code：{}", sampleCode);
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
            } else {
                log.info("该code下无信息-Code：{}", sampleCode);
                return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NO_MEG);
            }
        } else if (entity instanceof BuySample) {
            return getDetailBuy((BuySample) entity);
        } else if (entity instanceof SaleSample) {
            log.info("该样品属于销售样品,SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NO_MEG);
        } else if (entity instanceof ProSample) {
            return getDetailPro((ProSample) entity);
        } else if (entity instanceof ProBagging) {
            return getDetailBag((ProBagging) entity);
        } else {
            log.info("未在采购/销售/生产样品/成品装袋中找到该code, SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
    }

    /**
     * 根据编码查询详情-采购
     */
    private ResponseResult getDetailBuy(BuySample sample) {
        DetailBySampleCodeBuyDTO dto = BeanUtils.copyProperties(sample, DetailBySampleCodeBuyDTO.class);
        dto.setSampleType(EnumSampleRelation.CodeScanSampleType.PURCHASE.getKey());
        // 化验结果
        if (StringUtils.isBlank(sample.getSampleCode())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
        }
        List<SampleAssayResult> sampleAssayResultList = sampleAssayResultService.selectList(new EntityWrapper<>(new SampleAssayResult()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sample.getSampleCode())
                .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())));
        if (!CollectionUtils.isEmpty(sampleAssayResultList)) {
            dto.setAssayResultList(BeanUtils.assemble(SampleAssayResultOutDTO.class, sampleAssayResultList));
        } else {
            dto.setAssayResultList(new ArrayList<>(0));
        }
        // 合同相关
        List<DetailContractStorageBuyDTO> detailList = buySampleWeightRelationMapper.getContractStorageDetailList(sample.getSampleCode());
        dto.setContractStorageDetailList(detailList == null ? new ArrayList<>(0) : detailList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * 根据编码查询详情-生产
     */
    private ResponseResult getDetailPro(ProSample sample) {
        DetailBySampleCodeProDTO dto = BeanUtils.copyProperties(sample, DetailBySampleCodeProDTO.class);
        dto.setSampleType(EnumSampleRelation.CodeScanSampleType.PRODUCE.getKey());
        // 化验结果
        if (StringUtils.isBlank(sample.getSampleCode())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
        }
        List<SampleAssayResult> sampleAssayResultList = sampleAssayResultService.selectList(new EntityWrapper<>(new SampleAssayResult()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sample.getSampleCode())
                .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())));
        if (!CollectionUtils.isEmpty(sampleAssayResultList)) {
            dto.setAssayResultList(BeanUtils.assemble(SampleAssayResultOutDTO.class, sampleAssayResultList));
        } else {
            dto.setAssayResultList(new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * 根据编码查询详情-成品装袋
     */
    private ResponseResult getDetailBag(ProBagging bag) {
        DetailBySampleCodeBagDTO dto = BeanUtils.copyProperties(bag, DetailBySampleCodeBagDTO.class);
        dto.setSampleType(EnumSampleRelation.CodeScanSampleType.BAG.getKey())
                .setBagTime(bag.getCreateTime());
        // 仓库信息
        if (!StringUtils.isBlank(bag.getBaggingCode())) {
            StorageIdAndNameDTO nameDTO = finishedProductBeOutOfStorageMapper.getStorageIdAndNameByTonCode(bag.getBaggingCode());
            if (nameDTO != null) {
                dto.setStorageId(nameDTO.getStorageId()).setStorageName(nameDTO.getStorageName());
            }
        }
        // 质检信息
        String sampleCode = bag.getSampleCode();
        if (StringUtils.isBlank(sampleCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
        }
        ProSample sample = proSampleService.selectOne(new EntityWrapper<>(new ProSample()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sampleCode)));
        if (sample != null) {
            dto.setSampleId(sample.getProductId())
                    .setSampleName(sample.getProductName())
                    .setSampleSpecId(sample.getProductSpecId())
                    .setSampleSpecName(sample.getProductSpecName())
                    .setAssayId(sample.getAssayId())
                    .setAssayPerson(sample.getAssayPerson())
                    .setAssayTime(sample.getAssayTime());
        }
        // 化验结果
        List<SampleAssayResult> sampleAssayResultList = sampleAssayResultService.selectList(new EntityWrapper<>(new SampleAssayResult()
                .setStatus(Status.TRUE.getKey())
                .setSampleCode(sampleCode)
                .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())));
        if (!CollectionUtils.isEmpty(sampleAssayResultList)) {
            dto.setAssayResultList(BeanUtils.assemble(SampleAssayResultOutDTO.class, sampleAssayResultList));
        } else {
            dto.setAssayResultList(new ArrayList<>(0));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}
