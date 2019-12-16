package com.bee.platform.cloud.si.manufacture.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumContract;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumReportForm;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumProBagging;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.ConfigReportFieldsService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtil;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
import com.google.common.collect.Lists;
import io.netty.util.internal.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.joda.time.LocalDate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 报表字段配置表 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-10-18
 */
@Slf4j
@Service
public class ConfigReportFieldsServiceImpl extends ServiceImpl<ConfigReportFieldsMapper, ConfigReportFields> implements ConfigReportFieldsService {
    @Autowired
    private ConfigProductCategoryMapper productCategoryMapper;
    @Autowired
    private ProSampleMapper proSampleMapper;
    @Autowired
    private SampleAssayResultMapper assayResultMapper;
    @Autowired
    private BuySampleMapper buySampleMapper;
    @Autowired
    private SaleSampleMapper saleSampleMapper;
    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;
    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;
    @Autowired
    private ConfigReportFieldsMapper reportFieldsMapper;
    @Autowired
    private StorageInventoryMapper storageInventoryMapper;
    @Autowired
    private BuyGoodsPendingStorageMapper buyGoodsPendingStorageMapper;
    @Autowired
    private ConfigDeviceMapper configDeviceMapper;
    @Autowired
    private ConfigProductSpecMapper configProductSpecMapper;
    @Autowired
    private FinishedProductOutStorageDetailMapper productOutStorageDetailMapper;
    @Autowired
    private ProBaggingMapper proBaggingMapper;
    @Autowired
    private ConfigProductMapper configProductMapper;
    @Autowired
    private ProductionOutStorageDetailMapper productionOutStorageDetailMapper;
    @Autowired
    private ConfigRawMaterialLossMapper configRawMaterialLossMapper;
    @Autowired
    private ConfigProductTestAttributeOutMapper configProductTestAttributeOutMapper;
    @Autowired
    private ProBlankingDetailMapper proBlankingDetailMapper;
    @Autowired
    private FreeStorageDetailMapper freeStorageDetailMapper;
    @Autowired
    private PickOutStorageDetailMapper pickOutStorageDetailMapper;
    @Autowired
    private FinishedProductOutStorageDetailMapper finishedProductOutStorageDetailMapper;

    /**
     * 月份
     */
    private static final String[] monthList = {"1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"};
    private static final String frontId = "frontSpec";
    private static final String afterId = "afterSpec";
    private static final String allId = "allSpec";
    private static final String itemOut = "itemOut";

    private static final Integer ONE = 1;
    private static final Integer ZERO = 0;

    @Override
    public ResponseResult<ReportFormFieldsTotalDTO> getReportFormFields(AuthPlatformUserInfo userInfo, String reportType, Integer productId,Integer businessType) {

        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        ReportFormFieldsTotalDTO result = new ReportFormFieldsTotalDTO();

        //固定字段
        List<ConfigReportFields> fields = reportFieldsMapper.selectList(new EntityWrapper<ConfigReportFields>()
                .eq("type", reportType)
                .eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
        List<ReportFormFieldsFixedDTO> fieldsFixed = new ArrayList<>();
        if(!CollectionUtils.isEmpty(fields)){
            for (ConfigReportFields field:fields) {
                ReportFormFieldsFixedDTO formFields = new ReportFormFieldsFixedDTO();
                formFields.setDataIndex(field.getFields());
                formFields.setTitle(field.getFieldsName());
                fieldsFixed.add(formFields);
            }
        }
        result.setFieldsFixed(fieldsFixed);
        //动态字段
        List<String> codes = null;
        //productId为空 直接返回
        if(ObjectUtils.isEmpty(productId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
        }
        EntityWrapper<SampleAssayResult> wrapper = new EntityWrapper<>();
        switch (businessType) {
                //采购
            case 1:
                codes = getBuyCodes(productId,orgId,factoryId);
                wrapper.eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey());
                break;
                //销售
            case 2:
                codes = getSaleCodes(productId,orgId,factoryId);
                wrapper.eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey());
                break;
                //生产
            case 3:
                codes = getProduceCodes(productId,orgId,factoryId);
                wrapper.eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey());
                break;
                //进出厂：采购+销售
            case 4:
                List<String> buyCodes = getBuyCodes(productId,orgId,factoryId);
                List<String> saleCodes = getSaleCodes(productId,orgId,factoryId);
                if(!CollectionUtils.isEmpty(buyCodes)||!CollectionUtils.isEmpty(saleCodes)){
                    codes = CollectionUtils.isEmpty(buyCodes)?saleCodes:getUnionList(buyCodes,saleCodes);
                }
                wrapper.ne("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey());
                break;
            default:
                throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        //化验结果输出项
        Integer type = Integer.parseInt(reportType);
        Integer buyKey = EnumReportForm.REPORT_TYPE.BUY.getKey();
        Integer saleKey = EnumReportForm.REPORT_TYPE.SALE.getKey();
        Integer detailKey = EnumReportForm.REPORT_TYPE.EXISTING_DETAILS.getKey();
        Integer dailyKey = EnumReportForm.REPORT_TYPE.MATERIAL_DAILY.getKey();
        Integer checkoutKey = EnumReportForm.REPORT_TYPE.PRODUCE_CHECKOUT.getKey();
        ArrayList<Integer> keys = Lists.newArrayList(buyKey, saleKey,detailKey,dailyKey,checkoutKey);
        //筛除采购、销售、现存明细表【不含化验输出项】
        if(!keys.contains(type) && !CollectionUtils.isEmpty(codes)){
            List<SampleAssayResult> sampleResults = assayResultMapper.selectList(wrapper
                    .in("sample_code", codes)
                    .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                    .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
            if(!CollectionUtils.isEmpty(sampleResults)){
                List<String> lists = Lists.newArrayList();
                //化验输出项去重
                for (SampleAssayResult assayResult : sampleResults) {
                    String item = assayResult.getAssayItem();
                    lists.add(item);
                }
                List<String> assayItems = lists.stream().distinct().collect(Collectors.toList());
                List<Map<String,Object>> assayItemResult = new ArrayList<>();
                for (String str:assayItems) {
                    Map<String,Object> map = new HashMap<>(2);
                    map.put("item",str);
                    assayItemResult.add(map);
                }
                result.setAssayItem(assayItemResult);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    /**
     * 并集【A不为空】
     * @param a
     * @param b
     * @return
     */
    private List<String> getUnionList(List<String> a,List<String> b){
        if(!CollectionUtils.isEmpty(b)){
            a.addAll(b);
        }
        return a;
    }

    /**
     * 采购产品相关样品codes
     * @param productId
     * @param orgId
     * @param factoryId
     * @return
     */
    private List<String> getBuyCodes(Integer productId,Integer orgId, Integer factoryId){
        List<String> codes = null;
        //采购
        List<BuySample> samples = buySampleMapper.selectList(new EntityWrapper<BuySample>()
                .eq("product_id", productId)
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .ne("assay_status", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(samples)){
            codes = samples.stream().map(e -> e.getSampleCode()).collect(Collectors.toList());
        }
        return codes;
    }
    /**
     * 销售产品相关样品codes
     * @param productId
     * @param orgId
     * @param factoryId
     * @return
     */
    private List<String> getSaleCodes(Integer productId,Integer orgId, Integer factoryId){
        List<String> codes = null;
        //销售
        List<SaleSample> samples = saleSampleMapper.selectList(new EntityWrapper<SaleSample>()
                .eq("product_id", productId)
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .ne("assay_status", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(samples)){
            codes = samples.stream().map(e -> e.getSampleCode()).collect(Collectors.toList());
        }
        return codes;
    }
    /**
     * 生产产品相关样品codes
     * @param productId
     * @param orgId
     * @param factoryId
     * @return
     */
    private List<String> getProduceCodes(Integer productId,Integer orgId, Integer factoryId){
        List<String> codes = null;
        //生产
        List<ProSample> samples = proSampleMapper.selectList(new EntityWrapper<ProSample>()
                .eq("product_id", productId)
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .ne("assay_status", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(samples)){
            codes = samples.stream().map(e -> e.getSampleCode()).collect(Collectors.toList());
        }
        return codes;
    }

    @Override
    public ResponseResult<List<ReportFormProductCategoryDTO>> getProductCategories(AuthPlatformUserInfo userInfo) {
        List<ReportFormProductCategoryDTO> result = new ArrayList<>();
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<ConfigProductCategory> categories = productCategoryMapper.selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("deleted", Status.FALSE.getKey()));
        if(!CollectionUtils.isEmpty(categories)){
            categories.forEach(e -> result.add(new ReportFormProductCategoryDTO().setCategoryId(e.getId()).setCategoryName(e.getName())));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    public ResponseResult<ReportFormTestQualityTestTotalDTO> getQualityTestReportForm(AuthPlatformUserInfo userInfo,
                                                                ReportFormQualityTestRq rq, Pagination pagination) {
        ReportFormTestQualityTestTotalDTO data = new ReportFormTestQualityTestTotalDTO();
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<ReportFormTestQualityTestDTO> result = new ArrayList<>();
        //生产质检
        if(EnumReportForm.TEST_TYPE.PRODUCE.getKey().equals(rq.getType())){
            EntityWrapper<ProSample> wrapper = new EntityWrapper<>();
            if(!ObjectUtils.isEmpty(rq.getSampleCode())){
                wrapper.like("sample_code",rq.getSampleCode());
            }
            if(!ObjectUtils.isEmpty(rq.getProductId())){
                wrapper.eq("product_id",rq.getProductId());
            }
            if(!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())){
                wrapper.ge("create_time",rq.getStartTime() + DateUtils.TIME_SUFFIX)
                        .le("create_time",rq.getEndTime() + DateUtils.TIME_END);
            }
            //查询生产化验基础信息
            List<ProSample> proSamples = proSampleMapper.selectPage(pagination,wrapper
                    .eq("enterprise_id", orgId)
                    .eq("factory_id", factoryId)
                    .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                    .ne("assay_status", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                    .orderBy("assay_time", false));
            if(!CollectionUtils.isEmpty(proSamples)){
                result = BeanUtils.assemble(ReportFormTestQualityTestDTO.class,proSamples);
                //取样化验项
                for (ReportFormTestQualityTestDTO testDTO:result) {
                    //质检类型
                    testDTO.setBusinessTypeName(EnumReportForm.INSPECTION_TYPE.getValue(testDTO.getBusinessType()));
                    List<SampleAssayResult> assayResults = assayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                            .eq("sample_code", testDTO.getSampleCode())
                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())
                            .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
                    if(!CollectionUtils.isEmpty(assayResults)){
                        List<Map<String,Object>> assayItem = new ArrayList<>();
                        for (SampleAssayResult assayResult:assayResults) {
                            Map<String,Object> param = new HashMap<>(2);
                            param.put(assayResult.getAssayItem(),assayResult.getAssayValue());
                            assayItem.add(param);
                        }
                        testDTO.setItems(assayItem);
                    }
                }
            }
            //进出场质检
        }else{
            Map<String,Object> map = new HashMap<>(6);
            if(!ObjectUtils.isEmpty(rq.getSampleCode())){
                map.put("sampleCode",rq.getSampleCode());
            }
            if(!ObjectUtils.isEmpty(rq.getCategoryId()) && !ZERO.toString().equals(rq.getCategoryId())){
                map.put("categoryId",rq.getCategoryId());
            }
            if(!ObjectUtils.isEmpty(rq.getProductId())){
                map.put("productId",rq.getProductId());
            }
            if(!ObjectUtils.isEmpty(rq.getStartTime())){
                map.put("startTime",rq.getStartTime() + DateUtils.TIME_SUFFIX);
                map.put("endTime",rq.getEndTime() + DateUtils.TIME_END);
            }
            map.put("enterpriseId",orgId);
            map.put("factoryId",factoryId);
            map.put("status",EnumCommon.LogicStatus.NORMAL.getKey());
            map.put("assayStatus", EnumSampleRelation.SampleAssayStatus.ABANDON.getKey());
            result = buySampleMapper.getSampleInfoINOutFactory(pagination,map);
            if(!CollectionUtils.isEmpty(result)){
                EnumSampleRelation.QualityAssayResult noStatus = EnumSampleRelation.QualityAssayResult.NO;
                EnumSampleRelation.QualityAssayResult yesStatus = EnumSampleRelation.QualityAssayResult.YES;
                //取样化验项
                for (ReportFormTestQualityTestDTO testDTO:result) {
                    //质检类型
                    testDTO.setBusinessTypeName(EnumReportForm.INSPECTION_TYPE.getValue(testDTO.getBusinessType()));
                    //合格
                    if(!ObjectUtils.isEmpty(testDTO.getAssayResultInt())){
                        testDTO.setAssayResult(yesStatus.getKey().equals(testDTO.getAssayResultInt())?yesStatus.getValue():noStatus.getValue());
                    }
                    List<SampleAssayResult> assayResults = assayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                            .eq("sample_code", testDTO.getSampleCode())
                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .ne("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())
                            .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
                    if(!CollectionUtils.isEmpty(assayResults)){
                        List<Map<String,Object>> assayItem = new ArrayList<>();
                        for (SampleAssayResult sampleAssayResult:assayResults) {
                            Map<String,Object> param = new HashMap<>(assayResults.size());
                            param.put(sampleAssayResult.getAssayItem(),sampleAssayResult.getAssayValue());
                            assayItem.add(param);
                        }
                        testDTO.setItems(assayItem);
                    }
                }
            }
        }
        data.setData(result);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,data, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<ReportFormBuyTotalDTO> getBuyReportForm(AuthPlatformUserInfo userInfo, ReportFormBuyRq rq, Pagination pagination) {

        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<ReportFormBuyDTO> list;
        ReportFormBuyTotalDTO result = new ReportFormBuyTotalDTO();

        EntityWrapper<BuyContractBasic> wrapper = new EntityWrapper<>();
        if(!ObjectUtils.isEmpty(rq.getSupplierName())){
            wrapper.like("supplier_name",rq.getSupplierName());
        }
        if(!ObjectUtils.isEmpty(rq.getProductId())){
            wrapper.eq("product_id",rq.getProductId());
        }
        if(!ObjectUtils.isEmpty(rq.getContractNum())){
            wrapper.like("contract_num",rq.getContractNum());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())){
            wrapper
                    .ge("sign_date",rq.getStartTime()+ DateUtils.TIME_SUFFIX)
                    .le("sign_date",rq.getEndTime()+ DateUtils.TIME_END);
        }
        //采购合同信息
        List<BuyContractBasic> buyContractBasics = buyContractBasicMapper.selectPage(pagination,wrapper
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(buyContractBasics)){
            list = BeanUtils.assemble(ReportFormBuyDTO.class, buyContractBasics);
            list.stream().forEach((e) -> {
                if(!ObjectUtils.isEmpty(e.getCompletedVolume())){
                    //结算数量：完成数量
                    e.setWeightSettle(e.getCompletedVolume());
                    //重量盈亏：结算数量-收货数量
                    e.setGainOrLossWeight(e.getCompletedVolume()
                            .subtract(ObjectUtils.isEmpty(e.getWeightReceive())?BigDecimal.ZERO:e.getWeightReceive())
                            .setScale(3, BigDecimal.ROUND_HALF_UP));
                }
                //未发货数量：合同工数量-已发数量
                e.setNotArrivalVolume(e.getUndeliveredVolume());
                //合同状态
                String completeStatus = EnumContract.IS_COMPLETED.getValue(e.getCompleted());
                e.setCompletedStatus(completeStatus);
            });
            result.setData(list);
            //合计相关
            ReportFormBuySumDTO sumBuy = assembleDataSumBuy(list);
            result.setDataSum(sumBuy);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * 采购报表统计值
     * @param list
     * @return
     */
    private ReportFormBuySumDTO assembleDataSumBuy(List<ReportFormBuyDTO> list){
        ReportFormBuySumDTO sum = new ReportFormBuySumDTO();
        //合同数量合计
        BigDecimal quantitySum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getQuantity()))
                .map(ReportFormBuyDTO::getQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setQuantity(quantitySum);
        //合同金额统计
        BigDecimal amountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmount()))
                .map(ReportFormBuyDTO::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmount(amountSum);
        //收货数量
        BigDecimal receiveSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightReceive()))
                .map(ReportFormBuyDTO::getWeightReceive)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setWeightReceive(receiveSum);
        //结算数量
        BigDecimal settleQuantitySum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightSettle()))
                .map(ReportFormBuyDTO::getWeightSettle)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setWeightSettle(settleQuantitySum);
        //结算金额
        BigDecimal settleAmountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlementTotal()))
                .map(ReportFormBuyDTO::getAmountSettlementTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmountSettlementTotal(settleAmountSum);
        //已付款金额
        BigDecimal payAmountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountPaymentTotal()))
                .map(ReportFormBuyDTO::getAmountPaymentTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmountPaymentTotal(payAmountSum);
        return sum;
    }

    @Override
    public ResponseResult<ReportFormSaleTotalDTO> getSaleReportForm(AuthPlatformUserInfo userInfo, ReportFormSaleRq rq, Pagination pagination){

        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<ReportFormSaleDTO> list;
        ReportFormSaleTotalDTO result = new ReportFormSaleTotalDTO();

        EntityWrapper<SaleContractBasic> wrapper = new EntityWrapper<>();
        if(!ObjectUtils.isEmpty(rq.getCustomerName())){
            wrapper.like("customer_name",rq.getCustomerName());
        }
        if(!ObjectUtils.isEmpty(rq.getProductId())){
            wrapper.eq("product_id",rq.getProductId());
        }
        if(!ObjectUtils.isEmpty(rq.getContractNum())){
            wrapper.like("contract_num",rq.getContractNum());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())){
            wrapper
                    .ge("sign_date",rq.getStartTime()+ DateUtils.TIME_SUFFIX)
                    .le("sign_date",rq.getEndTime()+ DateUtils.TIME_END);
        }
        //销售合同信息
        List<SaleContractBasic> buyContractBasics = saleContractBasicMapper.selectPage(pagination,wrapper
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(buyContractBasics)){
            list = BeanUtils.assemble(ReportFormSaleDTO.class, buyContractBasics);
            list.stream().forEach((e) -> {
                if(!ObjectUtils.isEmpty(e.getCompletedVolume())){
                    //结算数量：完成数量
                    e.setWeightSettle(e.getCompletedVolume());
                    //重量盈亏：结算数量-收货数量
                    e.setGainOrLossWeight(e.getCompletedVolume()
                            .subtract(ObjectUtils.isEmpty(e.getWeightReceive())?BigDecimal.ZERO:e.getWeightReceive())
                            .setScale(3, BigDecimal.ROUND_HALF_UP));
                }

                //合同状态
                String completeStatus = EnumContract.IS_COMPLETED.getValue(e.getCompleted());
                e.setCompletedStatus(completeStatus);
                //已发货数量
                e.setArrivalVolume(e.getIssuedVolume());
                //未发货数量
                e.setNotArrivalVolume(e.getUndeliveredVolume());
                //到厂数量
                e.setWeightReceive(e.getReceivedVolume());

            });
            result.setData(list);
            ReportFormSaleSumDTO sumSale = assembleDataSumSale(list);
            result.setDataSum(sumSale);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * 销售报表统计值
     * @param list
     * @return
     */
    private ReportFormSaleSumDTO assembleDataSumSale(List<ReportFormSaleDTO> list){

        ReportFormSaleSumDTO sum = new ReportFormSaleSumDTO();
        //合同数量合计
        BigDecimal quantitySum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getQuantity()))
                .map(ReportFormSaleDTO::getQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setQuantity(quantitySum);
        //合同金额统计
        BigDecimal amountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmount()))
                .map(ReportFormSaleDTO::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmount(amountSum);
        //收货数量
        BigDecimal receiveSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightReceive()))
                .map(ReportFormSaleDTO::getWeightReceive)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setWeightReceive(receiveSum);
        //结算数量
        BigDecimal settleQuantitySum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightSettle()))
                .map(ReportFormSaleDTO::getWeightSettle)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
        sum.setWeightSettle(settleQuantitySum);
        //结算金额
        BigDecimal settleAmountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlementTotal()))
                .map(ReportFormSaleDTO::getAmountSettlementTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmountSettlementTotal(settleAmountSum);
        //已付款金额
        BigDecimal collectionAmountSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountCollectionTotal()))
                .map(ReportFormSaleDTO::getAmountCollectionTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
        sum.setAmountCollectionTotal(collectionAmountSum);
        return sum;
    }

    @Override
    public ResponseResult<ReportFormProductWarehouseTotalDTO> getProductWarehouseReportForm(
                                        AuthPlatformUserInfo userInfo, ReportFormWarehouseRq rq, Pagination pagination) {

        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        ReportFormProductWarehouseTotalDTO result = new ReportFormProductWarehouseTotalDTO();

        Map<String,Object> map = new HashMap<>(6);

        if(!ObjectUtils.isEmpty(rq.getProductId())){
            map.put("productId",rq.getProductId());
        }
        if(!ObjectUtils.isEmpty(rq.getStorageId()) && rq.getStorageId()!=0){
            map.put("depositoryId",rq.getStorageId());
            log.info("depositoryId", rq.getStorageId());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime())){
            map.put("startTime",rq.getStartTime() + DateUtils.TIME_SUFFIX);
            map.put("endTime",rq.getEndTime() + DateUtils.TIME_END);
        }
        map.put("enterpriseId",orgId);
        map.put("factoryId",factoryId);
        map.put("status",EnumCommon.LogicStatus.NORMAL.getKey());
        map.put("putStorage", EnumStorage.PutStorage.storage.getKey());
        List<ReportFormProductWarehouseDTO> list = productOutStorageDetailMapper.getProductWarehouseReportFormByPage(map,pagination);
        if(!CollectionUtils.isEmpty(list)){
            result.setData(list);
            //合计值
            BigDecimal weightSum = list.stream().filter(e -> !ObjectUtils.isEmpty(e.getTonWeight()))
                    .map(ReportFormProductWarehouseDTO::getTonWeight)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(3, BigDecimal.ROUND_HALF_UP);
            result.setDataSum(new ReportFormProductWarehouseSumDTO().setTonWeight(weightSum));
            //取样化验项
            for (ReportFormProductWarehouseDTO dto:list) {
                List<SampleAssayResult> assayResults = assayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                        .eq("sample_code", dto.getSampleCode())
                        .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                        .eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PRODUCE.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
                if(!CollectionUtils.isEmpty(assayResults)){
                    List<Map<String, Object>> assayItem = resolveAssayResult(assayResults);
                    dto.setItems(assayItem);
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * 合格率报表
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ReportFormDataInfoDTO> getPassRateReportForm(AuthPlatformUserInfo userInfo, ReportFormPassRateRq rq) {

        ReportFormDataInfoDTO reportFormPassRateDTO = new ReportFormDataInfoDTO();

        List<Map<String, Object>> passRateDataList = new ArrayList<>(12);

        //查询产品的合格线
        ConfigProductSpec productSpec = configProductSpecMapper.selectOne(new ConfigProductSpec()
                .setProductId(rq.getProductId()).setQualifiedLine(EnumReportForm.QualifiedLine.qualified.getKey())
                .setStatus(Status.TRUE.getKey()).setDeleted(Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(productSpec)) {
            log.error("查询不到产品的合格线！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.PRODUCT_SPEC_NOT_EXIST);
        }

        //查询矿热炉
        List<ConfigDevice> furnaceList = configDeviceMapper.selectList(new EntityWrapper<ConfigDevice>()
                .eq("type", 0).eq("status", Status.TRUE.getKey())
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(furnaceList)) {
            log.error("未查询到矿热炉！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.FURNACE_NOT_EXIST);
        }

        //查询合格率的标题信息
        reportFormPassRateDTO.setFields(getReportTitleFieldsInfo(productSpec, furnaceList));

        //遍历月份处理
        int monthCount = 1;
        for (String month : monthList) {
            Map<String, Object> data = new HashMap<>(30);
            data.put("month", month);

            //总产量
            BigDecimal totalOutput = BigDecimal.ZERO;
            //合格产品
            BigDecimal qualifiedYield = BigDecimal.ZERO;
            //合格率
            BigDecimal passRate = BigDecimal.ZERO;

            //查询矿热炉合格产品数量
            Map<String, BigDecimal> qualifiedAmount = getFurnaceAmount(userInfo, rq, productSpec.getSort(), monthCount, 1);
            //查询矿热炉不合格产品数量
            Map<String, BigDecimal> notQualifiedAmount = getFurnaceAmount(userInfo, rq, productSpec.getSort(), monthCount, 0);

            //遍历矿热炉处理数据
            int count = 1;
            for (ConfigDevice furnace : furnaceList) {
                //合格
                BigDecimal furnaceFrontAmount = ObjectUtils.isEmpty(qualifiedAmount.get(furnace.getId().toString())) ? BigDecimal.ZERO : qualifiedAmount.get(furnace.getId().toString());
                //不合格
                BigDecimal furnaceAfterAmount = ObjectUtils.isEmpty(notQualifiedAmount.get(furnace.getId().toString())) ? BigDecimal.ZERO : notQualifiedAmount.get(furnace.getId().toString());

                String furnaceFrontId = frontId + count;
                String furnaceAfterId = afterId + count;
                String furnaceAllId = allId + count;
                data.put(furnaceFrontId, furnaceFrontAmount);
                data.put(furnaceAfterId, furnaceAfterAmount);
                data.put(furnaceAllId, furnaceFrontAmount.add(furnaceAfterAmount));

                qualifiedYield = qualifiedYield.add(furnaceFrontAmount);
                totalOutput = totalOutput.add(furnaceFrontAmount).add(furnaceAfterAmount);

                count++;
            }

            data.put("totalOutput", totalOutput);
            data.put("qualifiedYield", qualifiedYield);
            if (totalOutput.compareTo(BigDecimal.ZERO) > 0 && qualifiedYield.compareTo(BigDecimal.ZERO) > 0) {
                passRate = qualifiedYield.multiply(new BigDecimal(100)).divide(totalOutput, 2, RoundingMode.HALF_UP);
            }
            data.put("passRate", passRate);
            passRateDataList.add(data);
            monthCount++;
        }
        reportFormPassRateDTO.setDataList(passRateDataList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormPassRateDTO);
    }

    /**
     * 根据月份查询各个矿热炉的合格/不合格产品数量
     * @param userInfo
     * @param rq
     * @param qualifiedLineSort
     * @param month
     * @param isQualified 0-不合格 1-合格
     * @return
     */
    private Map<String, BigDecimal> getFurnaceAmount(AuthPlatformUserInfo userInfo, ReportFormPassRateRq rq,
                                                     Integer qualifiedLineSort, Integer month, Integer isQualified) {
        Map<String, BigDecimal> result = new HashMap<>(12);
        Map<String, Object> param = new HashMap<>(10);
        param.put("enterpriseId", userInfo.getOrgId());
        param.put("factoryId", userInfo.getFactoryId());
        param.put("productId", rq.getProductId());
        param.put("month", month);
        param.put("year", rq.getYear());
        param.put("sort", qualifiedLineSort);
        param.put("isQualified", isQualified);
        List<ReportFormPassRateAmountDTO> amountDTOS = proBaggingMapper.getPassRateAmount(param);
        if (!CollectionUtils.isEmpty(amountDTOS)) {
            result = amountDTOS.stream().collect(Collectors.toMap(ReportFormPassRateAmountDTO::getFurnaceId, ReportFormPassRateAmountDTO::getAmount));
        }
        return result;
    }

    /**
     * 查询合格率标题信息
     * @param productSpec
     * @param furnaceList
     * @return
     */
    private List<ReportTitleFieldsDTO> getReportTitleFieldsInfo(ConfigProductSpec productSpec, List<ConfigDevice> furnaceList) {

        List<ReportTitleFieldsDTO> frontFieldsDtoS = new ArrayList<>();
        List<ReportTitleFieldsDTO> afterFieldsDtoS = new ArrayList<>();

        //标题信息
        List<ReportTitleFieldsDTO> fields = new ArrayList<>();
        String frontName = productSpec.getSpecName() + "以上";
        String afterName = productSpec.getSpecName() + "以下";
        String allName = "合计";
        //查询动态前端
        List<ConfigReportFields> frontFieldsList = this.selectList(new EntityWrapper<ConfigReportFields>()
                .eq("type", EnumReportForm.REPORT_TYPE.PASS_RATE.getKey())
                .eq("dynamic_content", EnumReportForm.DYNAMIC_CONTENT.FRONT.getKey()));
        if (!CollectionUtils.isEmpty(frontFieldsList)) {
            for (ConfigReportFields fields1 : frontFieldsList) {
                ReportTitleFieldsDTO reportTitleFieldsDTO = new ReportTitleFieldsDTO();
                reportTitleFieldsDTO.setDataIndex(fields1.getFields());
                reportTitleFieldsDTO.setTitle(fields1.getFieldsName());
                frontFieldsDtoS.add(reportTitleFieldsDTO);
            }
        }
        fields.addAll(frontFieldsDtoS);

        //遍历处理矿热炉标题
        Integer count = 1;
        for (ConfigDevice furnace : furnaceList) {
            //标题处理
            ReportTitleFieldsDTO fieldsDTO = new ReportTitleFieldsDTO();
            fieldsDTO.setTitle(furnace.getName());

            //副标题
            List<ReportTitleFieldsDTO> childrenFields = new ArrayList<>();
            ReportTitleFieldsDTO fieldsDTO1 = new ReportTitleFieldsDTO();
            fieldsDTO1.setDataIndex(frontId + count);
            fieldsDTO1.setTitle(frontName);
            childrenFields.add(fieldsDTO1);
            ReportTitleFieldsDTO fieldsDTO2 = new ReportTitleFieldsDTO();
            fieldsDTO2.setDataIndex(afterId + count);
            fieldsDTO2.setTitle(afterName);
            childrenFields.add(fieldsDTO2);
            ReportTitleFieldsDTO fieldsDTO3 = new ReportTitleFieldsDTO();
            fieldsDTO3.setDataIndex(allId + count);
            fieldsDTO3.setTitle(allName);
            childrenFields.add(fieldsDTO3);
            fieldsDTO.setChildren(childrenFields);
            fields.add(fieldsDTO);
            count++;
        }

        //查询动态后端
        List<ConfigReportFields> afterFieldsList = this.selectList(new EntityWrapper<ConfigReportFields>()
                .eq("type", EnumReportForm.REPORT_TYPE.PASS_RATE.getKey())
                .eq("dynamic_content", EnumReportForm.DYNAMIC_CONTENT.AFTER.getKey()));
        if (!CollectionUtils.isEmpty(afterFieldsList)) {
            for (ConfigReportFields fields2 : afterFieldsList) {
                ReportTitleFieldsDTO reportTitleFieldsDTO = new ReportTitleFieldsDTO();
                reportTitleFieldsDTO.setDataIndex(fields2.getFields());
                reportTitleFieldsDTO.setTitle(fields2.getFieldsName());
                afterFieldsDtoS.add(reportTitleFieldsDTO);
            }
        }
        fields.addAll(afterFieldsDtoS);
        return fields;
    }


    /**
     * @Description 报表-库存-现存明细表
     * @author chenxm66777123
     * @Date 2019/10/21 15:11
     * @version 1.0.0
     */
    @Override
    public ResponseResult<ReportFormExistingAllDTO> getExistingDeatilsReportForm(AuthPlatformUserInfo userInfo,ReportFormExistingDeatilsRq rq, Pagination pagination) {
        EntityWrapper wrapper = new EntityWrapper();
        wrapper.eq("status", Status.TRUE.getKey());
        //产品
        if (!ObjectUtils.isEmpty(rq.getProductId())) {
            wrapper.eq("product_id", rq.getProductId());
        }
        //仓库,0为全部不走这个逻辑
        if (!ObjectUtils.isEmpty(rq.getStorageId()) && !rq.getStorageId().equals(ZERO)) {
            wrapper.eq("storage_id", rq.getStorageId());
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.ge("modify_time", rq.getStartTime() + DateUtils.TIME_SUFFIX);
            wrapper.le("modify_time", rq.getEndTime() + DateUtils.TIME_END);
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.ge("modify_time", rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.le("modify_time", rq.getEndTime() + DateUtils.TIME_END);
        }
        List<StorageInventory> storageInventories = storageInventoryMapper.selectPage(pagination, wrapper);
        ReportFormExistingAllDTO reportFormExistingAllDTO = new ReportFormExistingAllDTO();
        if(CollectionUtils.isEmpty(storageInventories)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                    reportFormExistingAllDTO,PageUtils.transToPage(pagination));
        }

        List<ReportFormExistingDeatilsDTO> result = new ArrayList<>();
        ReportFormExistingTotalDTO reportFormExistingTotalDTO = new ReportFormExistingTotalDTO();
        BigDecimal total = BigDecimal.ZERO;
        for (StorageInventory  obj : storageInventories) {
            ReportFormExistingDeatilsDTO reportFormExistingDeatilsDTO = new ReportFormExistingDeatilsDTO().setProductName(obj.getProductName()).
                    setProductSpecName(obj.getProductSpecName())
                    .setStorageName(obj.getStorageName())
                    .setUpdateTime(DateUtil.convertDateToString(obj.getModifyTime()))
                    .setAmount(obj.getProductNumber());
            result.add(reportFormExistingDeatilsDTO);
            total = total.add(bigDecimalIsNull(obj.getProductNumber()));
        }
        reportFormExistingAllDTO.setData(result);
        reportFormExistingTotalDTO.setAmount(total);
        reportFormExistingAllDTO.setDataSum(reportFormExistingTotalDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormExistingAllDTO,PageUtils.transToPage(pagination));
    }

    /**
     * @Description 报表-库存-采购入库表
     * @author chenxm66777123
     * @Date 2019/10/21 15:11
     * @version 1.0.0
     */
    @Override
    public ResponseResult<ReportBuySendStorageAllDTO> getBuySendStorageReportForm(AuthPlatformUserInfo userInfo,
                                                                                     ReportFormExistingDeatilsRq rq, Pagination pagination) {

        EntityWrapper wrapper = new EntityWrapper();
        wrapper.eq("status", Status.TRUE.getKey());
        wrapper.eq("put_storage", Status.TRUE.getKey());
        //产品
        if (!ObjectUtils.isEmpty(rq.getProductId())) {
            wrapper.eq("product_id", rq.getProductId());
        }
        //仓库,0 为全部不走这个逻辑
        if (!ObjectUtils.isEmpty(rq.getStorageId()) && !rq.getStorageId().equals(ZERO)) {
            wrapper.eq("storage_id", rq.getStorageId());
        }
        //入库时间
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.ge("storage_time", rq.getStartTime() + DateUtils.TIME_SUFFIX);
            wrapper.le("storage_time", rq.getEndTime() + DateUtils.TIME_END );
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.ge("storage_time", rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.le("storage_time", rq.getEndTime()  + DateUtils.TIME_END );
        }
        List<ReportBuySendStorageDTO> results = new ArrayList<>();
        //查询结果
        List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectPage(pagination, wrapper);
        ReportBuySendStorageAllDTO reportBuySendStorageAllDTO = new ReportBuySendStorageAllDTO();
        if(CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,reportBuySendStorageAllDTO,PageUtils.transToPage(pagination));
        }
        BigDecimal total = BigDecimal.ZERO;
        for (BuyGoodsPendingStorage buyGoodsPendingStorage : buyGoodsPendingStorages) {
            BuyContractBasic buyContractBasic =  buyContractBasicMapper.selectOne(
                    new BuyContractBasic()
                            .setContractNum(buyGoodsPendingStorage.getContractId())
                            .setStatus(Status.TRUE.getKey()));
            String contractId = "";
            String singData = "";
            String contractBusinessId = "";
            if(!ObjectUtils.isEmpty(buyContractBasic)
                    && userInfo.getOrgId().equals(buyContractBasic.getEnterpriseId())){
                contractId = buyGoodsPendingStorage.getContractId();
                singData = DateUtil.date2Str(buyContractBasic.getSignDate());
                contractBusinessId  = buyContractBasic.getContractBusinessId();
            }
            ReportBuySendStorageDTO reportBuySendStorageDTO = new ReportBuySendStorageDTO();


            reportBuySendStorageDTO.setContractNum(contractId);
            reportBuySendStorageDTO.setContractSignDate(singData);
            reportBuySendStorageDTO.setStorageTime(DateUtil.convertDateToString(buyGoodsPendingStorage.getStorageTime()));

            reportBuySendStorageDTO.setProductName(buyGoodsPendingStorage.getProductName());
            reportBuySendStorageDTO.setProductSpecName(buyGoodsPendingStorage.getProductSpecName());
            reportBuySendStorageDTO.setStorageName(buyGoodsPendingStorage.getStorageName());
            total = total.add(bigDecimalIsNull(buyGoodsPendingStorage.getProductNumber()));
            reportBuySendStorageDTO.setSendStorageAmount(bigDecimalIsNull(buyGoodsPendingStorage.getProductNumber()));

            List<BuySample> buySamples =  buySampleMapper.selectList(new EntityWrapper<BuySample>(
                    ).where("status = 1 and contract_business_id ={0} and product_id = {1} and product_spec_id ={2}"
                    ,contractBusinessId
                    ,buyGoodsPendingStorage.getProductId()
                    ,buyGoodsPendingStorage.getProductSpecId())
            );

            if (!CollectionUtils.isEmpty(buySamples)) {
                //取样化验项
                for (BuySample buySample : buySamples) {
                    List<SampleAssayResult> assayResults =
                            assayResultMapper.selectList(
                                    new EntityWrapper<SampleAssayResult>().eq("sample_code", buySample.getSampleCode())
                                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                                            .eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey())
                                            .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
                    if (!CollectionUtils.isEmpty(assayResults)) {
                        List<Map<String, Object>> assayItem = resolveAssayResult(assayResults);
                        reportBuySendStorageDTO.setItems(assayItem);
                    }
                }
            }

            results.add(reportBuySendStorageDTO);
        }
        ReportBuySendStorageTotalDTO reportBuySendStorageTotalDTO = new ReportBuySendStorageTotalDTO();
        reportBuySendStorageTotalDTO.setSendStorageAmount(total);
        reportBuySendStorageAllDTO.setData(results);
        reportBuySendStorageAllDTO.setDataSum(reportBuySendStorageTotalDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,reportBuySendStorageAllDTO,PageUtils.transToPage(pagination));
    }

    /**
     * @Description 报表-原材料日报
     * @author chenxm66777123
     * @Date 2019/10/22 10:31
     * @version 1.0.0
     */
    @Override
    public ResponseResult<ReportRawMaterialAllDTO> getRawMaterialReportForm(
            AuthPlatformUserInfo userInfo, ReportRawMaterialRq rq, Pagination pagination) {
        String dayTime = "";
        //未传时间，则默认为今天
        if(ObjectUtils.isEmpty(rq.getDayTime())){
            dayTime = DateUtil.date2Str(new Date());
        }
        else{
            dayTime = DateUtil.date2Str(rq.getDayTime());
        }

        ConfigProduct configProduct = configProductMapper.selectOne(new ConfigProduct().setId(rq.getProductId()).setDeleted(Status.FALSE.getKey()));
        List<ReportRawMaterialDTO> results = new ArrayList<>();
        if(ObjectUtils.isEmpty(configProduct)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_PRODUCT);
        }
        //查询有效的规格
        List<ConfigProductSpec> configProductSpecs = configProductSpecMapper.selectPage(pagination,new EntityWrapper<>(
                new ConfigProductSpec().setProductId(rq.getProductId())
                        .setDeleted(Status.FALSE.getKey())
                        .setEnterpriseId(userInfo.getOrgId())
        ));

        BigDecimal totalOriginalStock = BigDecimal.ZERO;
        BigDecimal totalTodayIntoStock = BigDecimal.ZERO;
        BigDecimal totalTodayUsed = BigDecimal.ZERO;
        BigDecimal totalLoss = BigDecimal.ZERO;
        BigDecimal totalTodayStock = BigDecimal.ZERO;
        if (!CollectionUtils.isEmpty(configProductSpecs)) {
            //循环处理
            for (ConfigProductSpec configProductSpec : configProductSpecs) {
                ReportRawMaterialDTO reportRawMaterialDTO = resolveRawMaterial(dayTime, configProduct, configProductSpec.getId(), configProductSpec.getSpecName());
                results.add(reportRawMaterialDTO);
                totalOriginalStock = totalOriginalStock.add(reportRawMaterialDTO.getOriginalStock());
                totalTodayIntoStock = totalTodayIntoStock.add(reportRawMaterialDTO.getTodayIntoStock());
                totalTodayUsed = totalTodayUsed.add(reportRawMaterialDTO.getTodayUsed());
                totalLoss = totalLoss.add(reportRawMaterialDTO.getLoss());
                totalTodayStock = totalTodayStock.add(reportRawMaterialDTO.getTodayStock());
            }
        } else {
            ReportRawMaterialDTO reportRawMaterialDTO = resolveRawMaterial(dayTime, configProduct, null, "");
            pagination.setTotal(1);
            results.add(reportRawMaterialDTO);
            totalOriginalStock = reportRawMaterialDTO.getOriginalStock();
            totalTodayIntoStock = reportRawMaterialDTO.getTodayIntoStock();
            totalTodayUsed = reportRawMaterialDTO.getTodayUsed();
            totalLoss = reportRawMaterialDTO.getLoss();
            totalTodayStock = reportRawMaterialDTO.getTodayStock();
        }
        ReportRawMaterialAllDTO reportRawMaterialAllDTO = new ReportRawMaterialAllDTO();
        reportRawMaterialAllDTO.setData(results);

        ReportRawMaterialTotalDTO reportRawMaterialTotalDTO = new ReportRawMaterialTotalDTO();
        reportRawMaterialTotalDTO.setOriginalStock(totalOriginalStock);
        reportRawMaterialTotalDTO.setTodayIntoStock(totalTodayIntoStock);
        reportRawMaterialTotalDTO.setTodayUsed(totalTodayUsed);
        reportRawMaterialTotalDTO.setLoss(totalLoss);
        reportRawMaterialTotalDTO.setTodayStock(totalTodayStock);

        reportRawMaterialAllDTO.setDataSum(reportRawMaterialTotalDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,reportRawMaterialAllDTO,PageUtils.transToPage(pagination));
    }


    private ReportRawMaterialDTO resolveRawMaterial(String dayTime, ConfigProduct configProduct,Integer productSpecId,String productSpecName) {

        ReportRawMaterialDTO reportRawMaterialDTO = new ReportRawMaterialDTO();
        Map<String, Object> map = new HashMap<>();
        map.put("dayTime", dayTime);
        map.put("productId", configProduct.getId());
        if(!ObjectUtils.isEmpty(productSpecId)){
            map.put("productSpecId", productSpecId);
        }

        //获取原材料当日入库
        BigDecimal todayIntoStock = buyGoodsPendingStorageMapper.getTodayIntoStock(map);
        todayIntoStock = bigDecimalIsNull(todayIntoStock);
        //获取自入库的数据
        BigDecimal todayFreeIntoStock = freeStorageDetailMapper.getTodayIntoStock(map);
        todayFreeIntoStock = bigDecimalIsNull(todayFreeIntoStock);

        //获取生产出库当日用量
        BigDecimal todayUsed = productionOutStorageDetailMapper.getTodayUsed(map);
        todayUsed = bigDecimalIsNull(todayUsed);
        //获取自由出库当日用量
        BigDecimal todayPickUsed =  pickOutStorageDetailMapper.getTodayUsed(map);
        todayPickUsed = bigDecimalIsNull(todayPickUsed);


        BigDecimal todayIntoStockAfter = bigDecimalIsNull(buyGoodsPendingStorageMapper.getTodayIntoStockAfter(map));
        BigDecimal todayIntoUsedAfter = bigDecimalIsNull(productionOutStorageDetailMapper.getTodayUsedAfter(map));

        //自由出入库
        BigDecimal todayFreeIntoStockAfter = bigDecimalIsNull(freeStorageDetailMapper.getTodayIntoStockAfter(map));
        BigDecimal todayFreeIntoUsedAfter = bigDecimalIsNull(pickOutStorageDetailMapper.getTodayUsedAfter(map));

        //获取损失率
        ConfigRawMaterialLoss configRawMaterialLoss =
                configRawMaterialLossMapper.selectOne(
                        new ConfigRawMaterialLoss()
                                .setProductId(configProduct.getId())
                                .setDeleted(Status.FALSE.getKey()));

        BigDecimal lossRatio = BigDecimal.ZERO;
        if(!ObjectUtils.isEmpty(configRawMaterialLoss)){
            lossRatio = configRawMaterialLoss.getLoss();
        }

        //当日总入库
        BigDecimal todayIntoStockAll = todayIntoStock.add(todayFreeIntoStock);
        //当日总出库
        BigDecimal todayUsedAll = todayUsed.add(todayPickUsed);

        //损耗
        BigDecimal loss = todayUsedAll.multiply(lossRatio).setScale(3,BigDecimal.ROUND_HALF_UP);
        loss = bigDecimalIsNull(loss);

        //现有库存
        BigDecimal  nowStock = storageInventoryMapper.getOriginalStock(map);
        nowStock = bigDecimalIsNull(nowStock);

        //原有库存  现有库存 + 当日用量 - 当日入库
        BigDecimal originalStock = nowStock.add(todayUsedAll)
                .add(todayIntoUsedAfter).add(todayFreeIntoUsedAfter)
                .subtract(todayIntoStockAfter)
                .subtract(todayFreeIntoStockAfter)
                .subtract(todayIntoStockAll);

        //今日库存： 原有库存	+ 今日入库 - 今日用量 - 损耗
        BigDecimal todayStock = originalStock.add(todayIntoStockAll)
                .subtract(todayUsedAll)
                .subtract(loss);

        //产品
        reportRawMaterialDTO.setProductName(configProduct.getName());
        //规格
        reportRawMaterialDTO.setProductSpecName(productSpecName);
        //原有库存
        reportRawMaterialDTO.setOriginalStock(originalStock);
        //今日入库
        reportRawMaterialDTO.setTodayIntoStock(todayIntoStock);
        //今日用量
        reportRawMaterialDTO.setTodayUsed(todayUsed);
        //损耗
        reportRawMaterialDTO.setLoss(loss);
        //今日库存
        reportRawMaterialDTO.setTodayStock(todayStock);

        return reportRawMaterialDTO;
    }


    /**
     * 产量分析报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    @Override
    public ResponseResult<ReportFormDataInfoDTO> getYieldAnalysisReportForm(AuthPlatformUserInfo userInfo, ReportFormYieldAnalysisRq rq,
                                                                            Pagination pagination) {

        ReportFormDataInfoDTO reportFormDataInfoDTO = new ReportFormDataInfoDTO();

        //查询产品的化验输出项
        List<ConfigProductTestAttributeOut> productTestAttributeOuts = configProductTestAttributeOutMapper.selectList(
                new EntityWrapper<ConfigProductTestAttributeOut>().eq("product_id", rq.getProductId())
                        .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(productTestAttributeOuts)) {
            log.error("该产品没有化验输出项！");
            List<ReportTitleFieldsDTO> fieldsDTOS = getYieldAnalysisReportTitleFieldsInfo(productTestAttributeOuts);
            reportFormDataInfoDTO.setFields(fieldsDTOS);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormDataInfoDTO);
        }

        //查询报表标题信息
        List<ReportTitleFieldsDTO> fieldsDTOS = getYieldAnalysisReportTitleFieldsInfo(productTestAttributeOuts);
        reportFormDataInfoDTO.setFields(fieldsDTOS);

        //查询产量信息
        Map<String, Object> param = new HashMap<>(10);
        param.put("productId", rq.getProductId());
        if (!ObjectUtils.isEmpty(rq.getFurnaceId()) && rq.getFurnaceId() != 0) {
            param.put("furnaceId", rq.getFurnaceId());
        }
        if (!ObjectUtils.isEmpty(rq.getShiftCode()) && rq.getShiftCode() != 0) {
            param.put("shiftCode", rq.getShiftCode());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime())){
            param.put("startTime", rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if(!ObjectUtils.isEmpty(rq.getEndTime())){
            param.put("endTime", rq.getEndTime() + DateUtils.TIME_END);
        }
        param.put("sort", rq.getSort());
        param.put("enterpriseId", userInfo.getOrgId());
        param.put("factoryId", userInfo.getFactoryId());
        List<ReportFormYieldAnalysisDTO> yieldAnalysisDTOS = proBaggingMapper.getFurnaceBatchAmount(param, pagination);

        List<Map<String, Object>> dataList = new ArrayList<>();

        //遍历查询化验输出项
        if (!CollectionUtils.isEmpty(yieldAnalysisDTOS)) {
            for (ReportFormYieldAnalysisDTO yieldAnalysisDTO : yieldAnalysisDTOS) {

                Map<String, Object> dataMap = JSON.parseObject(JSON.toJSONString(yieldAnalysisDTO));

                //化验输出项处理
                int count = 1;
                for (ConfigProductTestAttributeOut attributeOut : productTestAttributeOuts) {
                    String assayItemOut = itemOut + count;
                    String assayItemOutValue = StringUtil.EMPTY_STRING;
                    //查询输出项的具体值
                    SampleAssayResult assayResult = assayResultMapper.selectOne(new SampleAssayResult()
                            .setType(EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .setAssayItem(attributeOut.getAssayItemOut()).setSampleCode(yieldAnalysisDTO.getSampleCode())
                            .setStatus(Status.TRUE.getKey()));
                    if (!ObjectUtils.isEmpty(assayResult)) {
                        //判断单位
                        if (EnumSampleRelation.ProductUnit.PERCENTAGE.getKey().equals(assayResult.getTestUnit())) {
                            assayItemOutValue = assayResult.getAssayValue().toString() + EnumSampleRelation.ProductUnit.PERCENTAGE.getValue();
                        } else if (EnumSampleRelation.ProductUnit.EXTREME_RATIO.getKey().equals(assayResult.getTestUnit())) {
                            assayItemOutValue = assayResult.getAssayValue().toString() + EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue();
                        } else {
                            assayItemOutValue = assayResult.getAssayValue().toString();
                        }
                    }
                    dataMap.put(assayItemOut, assayItemOutValue);
                    count++;
                }
                dataList.add(dataMap);
            }
        }
        reportFormDataInfoDTO.setDataList(dataList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormDataInfoDTO, PageUtils.transToPage(pagination));
    }

    /**
     * 报表-库存-成品出库表
     * @author chenxm66777123
     * @Date 2019/10/22 10:22
     * @version 1.0.0
     */
    @Override
    public ResponseResult<ReportProductOutStorageAllDTO> getProductOutStorageReportForm(
            AuthPlatformUserInfo userInfo, ReportFormExistingDeatilsRq rq, Pagination pagination) {
        Map<String,Object> wrapper = new HashMap<>();
        wrapper.put("enterpriseId",userInfo.getOrgId());
        wrapper.put("factoryId",userInfo.getFactoryId() );
        //产品
        if (!ObjectUtils.isEmpty(rq.getProductId())) {
            wrapper.put("productId", rq.getProductId());
        }
        //仓库,0 为全部不走这个逻辑
        if (!ObjectUtils.isEmpty(rq.getStorageId()) && !rq.getStorageId().equals(ZERO)) {
            wrapper.put("storageId", rq.getStorageId());
        }
        //出库时间
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.put("startTime", rq.getStartTime() + DateUtils.TIME_SUFFIX);
            wrapper.put("endTime", rq.getEndTime() +  DateUtils.TIME_END);
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.put("startTime", rq.getStartTime()+  DateUtils.TIME_SUFFIX);
        }
        if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.put("endTime", rq.getEndTime()+  DateUtils.TIME_END);
        }


        ReportProductOutStorageAllDTO reportProductOutStorageAllDTO = new ReportProductOutStorageAllDTO();

        List<ReportProductOutStorageDTO> finishedProductOutStorageDetails
                = finishedProductOutStorageDetailMapper.getReportProductOutStorage(wrapper,pagination);


        if(CollectionUtils.isEmpty(finishedProductOutStorageDetails)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,reportProductOutStorageAllDTO,PageUtils.transToPage(pagination));
        }
        List<ReportProductOutStorageDTO> results = new ArrayList<>();
        BigDecimal totalOutStorageNumber = BigDecimal.ZERO;
        for (ReportProductOutStorageDTO productOutStorageDTO : finishedProductOutStorageDetails) {

            totalOutStorageNumber = totalOutStorageNumber.add(bigDecimalIsNull(productOutStorageDTO.getOutStorageNumber()));
            List<SaleSample> saleSamples =  saleSampleMapper.selectList(new EntityWrapper<SaleSample>(
                    new SaleSample().setProductSpecId(productOutStorageDTO.getProductSpecId())
                            .setStatus(Status.TRUE.getKey())
                            .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                            .setEnterpriseId(userInfo.getOrgId())
                            .setProductId(rq.getProductId())
                    ).like("assay_time", productOutStorageDTO.getOutStorageTime())
            );

            if (!CollectionUtils.isEmpty(saleSamples)) {
                //取样化验项
                for (SaleSample saleSample : saleSamples) {
                    List<SampleAssayResult> assayResults =
                            assayResultMapper.selectList(
                                    new EntityWrapper<SampleAssayResult>().eq("sample_code", saleSample.getSampleCode())
                                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                                            .eq("business_type", EnumSampleRelation.SampleAssayResultBusinessType.SALE.getKey())
                                            .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
                    if (!CollectionUtils.isEmpty(assayResults)) {
                        List<Map<String, Object>> assayItem = resolveAssayResult(assayResults);
                        productOutStorageDTO.setItems(assayItem);
                    }
                }
            }
            results.add(productOutStorageDTO);
        }
        reportProductOutStorageAllDTO.setData(results);

        ReportProductOutStorageTotalDTO reportProductOutStorageTotalDTO = new ReportProductOutStorageTotalDTO();
        reportProductOutStorageTotalDTO.setOutStorageNumber(totalOutStorageNumber);
        reportProductOutStorageAllDTO.setDataSum(reportProductOutStorageTotalDTO);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,reportProductOutStorageAllDTO,PageUtils.transToPage(pagination));
    }

    /**
     * 查询产量分析报表标题信息
     * @param productAttributeOuts
     * @return
     */
    private List<ReportTitleFieldsDTO> getYieldAnalysisReportTitleFieldsInfo(List<ConfigProductTestAttributeOut> productAttributeOuts) {

        List<ReportTitleFieldsDTO> frontFieldsDtoS = new ArrayList<>();
        List<ReportTitleFieldsDTO> afterFieldsDtoS = new ArrayList<>();

        //查询动态前端
        List<ConfigReportFields> frontFieldsList = this.selectList(new EntityWrapper<ConfigReportFields>()
                .eq("type", EnumReportForm.REPORT_TYPE.YIELD_ANALYSIS.getKey())
                .eq("dynamic_content", EnumReportForm.DYNAMIC_CONTENT.FRONT.getKey()));
        if (!CollectionUtils.isEmpty(frontFieldsList)) {
            for (ConfigReportFields fields : frontFieldsList) {
                ReportTitleFieldsDTO reportTitleFieldsDTO = new ReportTitleFieldsDTO();
                reportTitleFieldsDTO.setDataIndex(fields.getFields());
                reportTitleFieldsDTO.setTitle(fields.getFieldsName());
                frontFieldsDtoS.add(reportTitleFieldsDTO);
            }
        }

        //标题信息
        List<ReportTitleFieldsDTO> fields = new ArrayList<>(frontFieldsDtoS);

        //化验输出项处理
        int count = 1;
        if (!CollectionUtils.isEmpty(productAttributeOuts)) {
            for (ConfigProductTestAttributeOut attributeOut : productAttributeOuts) {
                ReportTitleFieldsDTO fieldsDTO = new ReportTitleFieldsDTO();
                fieldsDTO.setDataIndex(itemOut + count);
                fieldsDTO.setTitle(attributeOut.getAssayItemOut());
                fields.add(fieldsDTO);
                count++;
            }
        }

        //查询动态后端
        List<ConfigReportFields> afterFieldsList = this.selectList(new EntityWrapper<ConfigReportFields>()
                .eq("type", EnumReportForm.REPORT_TYPE.YIELD_ANALYSIS.getKey())
                .eq("dynamic_content", EnumReportForm.DYNAMIC_CONTENT.AFTER.getKey()));
        if (!CollectionUtils.isEmpty(afterFieldsList)) {
            for (ConfigReportFields fields1 : afterFieldsList) {
                ReportTitleFieldsDTO reportTitleFieldsDTO = new ReportTitleFieldsDTO();
                reportTitleFieldsDTO.setDataIndex(fields1.getFields());
                reportTitleFieldsDTO.setTitle(fields1.getFieldsName());
                afterFieldsDtoS.add(reportTitleFieldsDTO);
            }
        }
        fields.addAll(afterFieldsDtoS);
        return fields;
    }

    /**
     * @Description 解析化验项
     * @author chenxm66777123
     * @Date 2019/10/23 14:02
     * @version 1.0.0
     */
    private List<Map<String, Object>> resolveAssayResult(List<SampleAssayResult> assayResults) {
        List<Map<String, Object>> assayItem = Lists.newArrayList();
        for (SampleAssayResult sampleAssayResult : assayResults) {
            Map<String, Object> param = new HashMap<>(assayResults.size());
            param.put(sampleAssayResult.getAssayItem(), sampleAssayResult.getAssayValue());
            assayItem.add(param);
        }
        return assayItem;
    }

    @Override
    public ResponseResult<List<ReportFurnacesDTO>> getFurnaces(AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        List<ReportFurnacesDTO> result = new ArrayList<>();
        List<ConfigDevice> devices = configDeviceMapper.selectList(new EntityWrapper<ConfigDevice>()
                .eq("enterprise_id", orgId)
                .eq("factory_id", factoryId)
                .eq("type", 0).eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("deleted", Status.FALSE.getKey()));
        if(!CollectionUtils.isEmpty(devices)){
            for (ConfigDevice device:devices) {
                ReportFurnacesDTO furnaces = new ReportFurnacesDTO();
                furnaces.setFurnacesId(device.getId());
                furnaces.setFurnacesName(device.getName());
                result.add(furnaces);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    public ResponseResult<Map<String, Object>> getOutputStatisticsReportFormFields(AuthPlatformUserInfo userInfo,ReportFormOutputStatisticsRq rq) {
        Map<String,Object> map = new HashMap<>(15);
        map.put("specName","规格");
        map.put("shiftAmount1","产量");
        map.put("shiftPercent1","百分比");
        map.put("shiftSum1","累计");
        map.put("shiftAmount2","产量");
        map.put("shiftPercent3","百分比");
        map.put("shiftSum2","累计");
        map.put("shiftAmount3","产量");
        map.put("shiftPercent3","百分比");
        map.put("shiftSum3","累计");
        map.put("furnaceAmount","产量");
        map.put("furnacePercent","百分比");
        map.put("furnaceSum","累计");

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,map);
    }

    @Override
    public ResponseResult<ReportFormDataInfoDTO> getOutputStatisticsReportForm(AuthPlatformUserInfo userInfo,
                                                                               ReportFormOutputStatisticsRq rq) {
        ReportFormDataInfoDTO reportFormDataInfoDTO = new ReportFormDataInfoDTO();

        //获取表头字段
        List<ReportTitleFieldsDTO> fields = getOutputStatisticsReportTitle(rq);
        reportFormDataInfoDTO.setFields(fields);

        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Map<String,Object> map = new HashMap<>(6);
        map.put("enterpriseId",orgId);
        map.put("factoryId",factoryId);
        map.put("status",EnumCommon.LogicStatus.NORMAL.getKey());
        map.put("productId",rq.getProductId());
        if(!ObjectUtils.isEmpty(rq.getFurnaceId()) && rq.getFurnaceId() != 0){
            map.put("furnaceId",rq.getFurnaceId());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime())){
            map.put("startTime",rq.getStartTime() + DateUtils.TIME_SUFFIX);
            map.put("endTime",rq.getEndTime() + DateUtils.TIME_END);
        }else{
            map.put("startTime", LocalDate.now() + " 00:00:00");
            map.put("endTime", LocalDate.now() + " 23:59:59");
        }
        //按规格 班次 炉号 产量数据
        List<ReportFormAmountByShift> shiftList = proBaggingMapper.getOutputAmountBySpecsAndShift(map);
        List<Map<String,Object>> shiftData = Lists.newArrayList();
        //班次相关
        if(!CollectionUtils.isEmpty(shiftList)){
            getOutputByShift(shiftList,shiftData,rq);
        }
        reportFormDataInfoDTO.setDataList(shiftData);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormDataInfoDTO);
    }

    /**
     * 统计生产产量统计报表表头字段
     * @param rq
     * @return
     */
    private List<ReportTitleFieldsDTO> getOutputStatisticsReportTitle(ReportFormOutputStatisticsRq rq) {

        Map<String,Object> map = new HashMap<>(15);
        map.put("specName","规格");
        map.put("shiftAmount1","产量");
        map.put("shiftPercent1","百分比");
        map.put("shiftSum1","累计");
        map.put("shiftAmount2","产量");
        map.put("shiftPercent3","百分比");
        map.put("shiftSum2","累计");
        map.put("shiftAmount3","产量");
        map.put("shiftPercent3","百分比");
        map.put("shiftSum3","累计");
        map.put("furnaceAmount","产量");
        map.put("furnacePercent","百分比");
        map.put("furnaceSum","累计");

        //标题信息
        List<ReportTitleFieldsDTO> fields = new ArrayList<>();

        ReportTitleFieldsDTO fieldsDTO1 = new ReportTitleFieldsDTO().setDataIndex("specName").setTitle("规格");
        fields.add(fieldsDTO1);

        ConfigDevice configDevice = configDeviceMapper.selectOne(new ConfigDevice().setId(rq.getFurnaceId()));
        String name = ObjectUtils.isEmpty(configDevice) ? "全部" : configDevice.getName();

        ReportTitleFieldsDTO fieldsDTO5 = new ReportTitleFieldsDTO().setTitle(name);
        List<ReportTitleFieldsDTO> fields4 = new ArrayList<>();
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("furnaceAmount").setTitle("产量"));
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("furnacePercent").setTitle("百分比"));
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("furnaceSum").setTitle("累计"));
        fieldsDTO5.setChildren(fields4);
        fields.add(fieldsDTO5);

        ReportTitleFieldsDTO fieldsDTO2 = new ReportTitleFieldsDTO().setTitle("一班");
        List<ReportTitleFieldsDTO> fields1 = new ArrayList<>();
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("shiftAmount1").setTitle("产量"));
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("shiftPercent1").setTitle("百分比"));
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("shiftSum1").setTitle("累计"));
        fieldsDTO2.setChildren(fields1);
        fields.add(fieldsDTO2);

        ReportTitleFieldsDTO fieldsDTO3 = new ReportTitleFieldsDTO().setTitle("二班");
        List<ReportTitleFieldsDTO> fields2 = new ArrayList<>();
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("shiftAmount2").setTitle("产量"));
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("shiftPercent2").setTitle("百分比"));
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("shiftSum2").setTitle("累计"));
        fieldsDTO3.setChildren(fields2);
        fields.add(fieldsDTO3);

        ReportTitleFieldsDTO fieldsDTO4 = new ReportTitleFieldsDTO().setTitle("三班");
        List<ReportTitleFieldsDTO> fields3 = new ArrayList<>();
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("shiftAmount3").setTitle("产量"));
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("shiftPercent3").setTitle("百分比"));
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("shiftSum3").setTitle("累计"));
        fieldsDTO4.setChildren(fields3);
        fields.add(fieldsDTO4);

        return fields;
    }

    @Override
    public ResponseResult<Map<String, Object>> getConsumptionAnalysisReportFormFields(AuthPlatformUserInfo userInfo, ReportFormOutputStatisticsRq rq) {
        Map<String,Object> map = new HashMap<>(15);
        map.put("productName","项目");
        map.put("consume1","期间消耗");
        map.put("tonnesConsumption1","吨耗");
        map.put("cumulative1","累计");
        map.put("consume2","期间消耗");
        map.put("tonnesConsumption2","吨耗");
        map.put("cumulative2","累计");
        map.put("consume3","期间消耗");
        map.put("tonnesConsumption3","吨耗");
        map.put("cumulative3","累计");
        map.put("consumeAll","期间消耗");
        map.put("tonnesConsumptionAll","吨耗");
        map.put("cumulativeAll","累计");

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,map);
    }

    /**
     * 消耗分析统计报表
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ReportFormDataInfoDTO> getConsumptionAnalysisReportForm(AuthPlatformUserInfo userInfo, ReportFormOutputStatisticsRq rq) {
        ReportFormDataInfoDTO reportFormDataInfoDTO = new ReportFormDataInfoDTO();

        //获取表头字段
        List<ReportTitleFieldsDTO> fields = getConsumptionAnalysisReportTitle(rq);
        reportFormDataInfoDTO.setFields(fields);

        List<Map<String, Object>> results = new ArrayList<>();

        //分班次查询产成品各原料消耗
        Map<String, Object> params = new HashMap<>(5);
        params.put("productId", rq.getProductId());
        if (!StringUtils.isEmpty(rq.getStartTime())) {
            params.put("startTime", rq.getStartTime() + DateUtils.TIME_SUFFIX);
        }
        if (!StringUtils.isEmpty(rq.getEndTime())) {
            params.put("endTime", rq.getEndTime() + DateUtils.TIME_END);
        }
        params.put("furnaceId", rq.getFurnaceId());
        List<ReportFormConsumptionAnalysisAmountDTO> analysisAmountDTOS = proBlankingDetailMapper.getConsumptionAnalysisAmount(params);
        if (CollectionUtils.isEmpty(analysisAmountDTOS)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormDataInfoDTO);
        }

        //查询时间段内各班次生产的产成品数量
        //全部
        BigDecimal productionAmountAll = proBaggingMapper.getCurrentTimeProductionAmount(params);
        //一班
        params.put("shiftCode", 1);
        BigDecimal productionAmount1 = proBaggingMapper.getCurrentTimeProductionAmount(params);
        //二班
        params.put("shiftCode", 2);
        BigDecimal productionAmount2 = proBaggingMapper.getCurrentTimeProductionAmount(params);
        //三班
        params.put("shiftCode", 3);
        BigDecimal productionAmount3 = proBaggingMapper.getCurrentTimeProductionAmount(params);

        //遍历处理原料其他数据
        for (ReportFormConsumptionAnalysisAmountDTO analysisAmountDTO : analysisAmountDTOS) {

            Map<String, Object> result = JSON.parseObject(JSON.toJSONString(analysisAmountDTO));

            result.put("consumeAll", analysisAmountDTO.getConsume1().add(analysisAmountDTO.getConsume2()).add(analysisAmountDTO.getConsume3()));

            //查询班次、炉号下，当前原料今年所有消耗相加的数量
            Map<String, Object> selectParams = new HashMap<>(6);
            selectParams.put("productId", rq.getProductId());
            if (!StringUtils.isEmpty(rq.getStartTime())) {
                selectParams.put("startTime", rq.getStartTime() + DateUtils.TIME_SUFFIX);
            }
            if (!StringUtils.isEmpty(rq.getEndTime())) {
                selectParams.put("endTime", rq.getEndTime() + DateUtils.TIME_END);
            }
            selectParams.put("furnaceId", rq.getFurnaceId());
            selectParams.put("stockProductId", analysisAmountDTO.getProductId());
            List<ReportFormConsumptionAnalysisAmountDTO> amountDTOS = proBlankingDetailMapper.getConsumptionAnalysisAmount(selectParams);

            //累计
            if (CollectionUtils.isEmpty(amountDTOS)) {
                result.put("cumulative1", 0);
                result.put("cumulative2", 0);
                result.put("cumulative3", 0);
                result.put("cumulativeAll", 0);
            } else {
                ReportFormConsumptionAnalysisAmountDTO amountDTO = amountDTOS.get(0);
                result.put("cumulative1", amountDTO.getConsume1());
                result.put("cumulative2", amountDTO.getConsume2());
                result.put("cumulative3", amountDTO.getConsume3());
                result.put("cumulativeAll", amountDTO.getConsume1().add(amountDTO.getConsume2()).add(amountDTO.getConsume3()));
            }

            //吨耗
            //一班
            if (productionAmount1.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal tonnesConsumption1 = analysisAmountDTO.getConsume1().divide(productionAmount1, 2, RoundingMode.HALF_UP);
                result.put("tonnesConsumption1", tonnesConsumption1);
            } else {
                result.put("tonnesConsumption1", 0);
            }
            //二班
            if (productionAmount2.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal tonnesConsumption2 = analysisAmountDTO.getConsume2().divide(productionAmount2, 2, RoundingMode.HALF_UP);
                result.put("tonnesConsumption2", tonnesConsumption2);
            } else {
                result.put("tonnesConsumption2", 0);
            }
            //三班
            if (productionAmount3.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal tonnesConsumption3 = analysisAmountDTO.getConsume3().divide(productionAmount3, 2, RoundingMode.HALF_UP);
                result.put("tonnesConsumption3", tonnesConsumption3);
            } else {
                result.put("tonnesConsumption3", 0);
            }
            //全部
            if (productionAmountAll.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal tonnesConsumptionAll = analysisAmountDTO.getConsume1().add(analysisAmountDTO.getConsume2())
                        .add(analysisAmountDTO.getConsume3()).divide(productionAmountAll, 2, RoundingMode.HALF_UP);
                result.put("tonnesConsumptionAll", tonnesConsumptionAll);
            } else {
                result.put("tonnesConsumptionAll", 0);
            }
            results.add(result);
        }
        reportFormDataInfoDTO.setDataList(results);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportFormDataInfoDTO);
    }

    /**
     * 统计消耗分析统计报表表头字段
     * @param rq
     * @return
     */
    private List<ReportTitleFieldsDTO> getConsumptionAnalysisReportTitle(ReportFormOutputStatisticsRq rq) {

        //标题信息
        List<ReportTitleFieldsDTO> fields = new ArrayList<>();

        ReportTitleFieldsDTO fieldsDTO1 = new ReportTitleFieldsDTO().setDataIndex("productName").setTitle("项目");
        fields.add(fieldsDTO1);

        ConfigDevice configDevice = configDeviceMapper.selectOne(new ConfigDevice().setId(rq.getFurnaceId()));
        String name = ObjectUtils.isEmpty(configDevice) ? "全部" : configDevice.getName();

        ReportTitleFieldsDTO fieldsDTO5 = new ReportTitleFieldsDTO().setTitle(name);
        List<ReportTitleFieldsDTO> fields4 = new ArrayList<>();
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("consumeAll").setTitle("期间消耗"));
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("tonnesConsumptionAll").setTitle("吨耗"));
        fields4.add(new ReportTitleFieldsDTO().setDataIndex("cumulativeAll").setTitle("累计"));
        fieldsDTO5.setChildren(fields4);
        fields.add(fieldsDTO5);

        ReportTitleFieldsDTO fieldsDTO2 = new ReportTitleFieldsDTO().setTitle("一班");
        List<ReportTitleFieldsDTO> fields1 = new ArrayList<>();
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("consume1").setTitle("期间消耗"));
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("tonnesConsumption1").setTitle("吨耗"));
        fields1.add(new ReportTitleFieldsDTO().setDataIndex("cumulative1").setTitle("累计"));
        fieldsDTO2.setChildren(fields1);
        fields.add(fieldsDTO2);

        ReportTitleFieldsDTO fieldsDTO3 = new ReportTitleFieldsDTO().setTitle("二班");
        List<ReportTitleFieldsDTO> fields2 = new ArrayList<>();
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("consume2").setTitle("期间消耗"));
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("tonnesConsumption2").setTitle("吨耗"));
        fields2.add(new ReportTitleFieldsDTO().setDataIndex("cumulative2").setTitle("累计"));
        fieldsDTO3.setChildren(fields2);
        fields.add(fieldsDTO3);

        ReportTitleFieldsDTO fieldsDTO4 = new ReportTitleFieldsDTO().setTitle("三班");
        List<ReportTitleFieldsDTO> fields3 = new ArrayList<>();
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("consume3").setTitle("期间消耗"));
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("tonnesConsumption3").setTitle("吨耗"));
        fields3.add(new ReportTitleFieldsDTO().setDataIndex("cumulative3").setTitle("累计"));
        fieldsDTO4.setChildren(fields3);
        fields.add(fieldsDTO4);

        return fields;
    }

    /**
     * 获取班次相关产量统计数据
     * @param shiftList
     * @param shiftData
     * @param rq
     */
    private void getOutputByShift(List<ReportFormAmountByShift> shiftList,List<Map<String,Object>> shiftData,ReportFormOutputStatisticsRq rq){
        Integer shiftOne = EnumProBagging.Shift.ONE.getKey();
        Integer shiftTwo = EnumProBagging.Shift.TWO.getKey();
        Integer shiftThree = EnumProBagging.Shift.THREE.getKey();
        for (ReportFormAmountByShift shift:shiftList) {
            Integer productSpecId = shift.getProductSpecId();
            //班次相关
            Map<String ,Object> shiftMap = new HashMap<>(5);
            shiftMap.put(EnumProBagging.SHIFT_AMOUNT.ONE.getValue(),shift.getShiftOne());
            //各规格 每班次总产量：截止当天，该规格、班次、炉号下，今年所有产量相加
            EntityWrapper<ProBagging> wrapper = new EntityWrapper<>();
            wrapper.eq("shift_code",shiftOne);
            wrapper.ge("modify_time",LocalDate.now().withDayOfYear(1)+ " 00:00:00");
            wrapper.le("modify_time",LocalDate.now()+ " 23:59:59");
            if(rq.getFurnaceId()!=null && rq.getFurnaceId()!=0){
                wrapper.eq("furnace_id",rq.getFurnaceId());
            }
            if(rq.getProductId()!=null && rq.getProductId()!=0){
                wrapper.eq("product_id",rq.getProductId());
            }
            wrapper.eq("status",EnumCommon.LogicStatus.NORMAL.getKey());
            wrapper.eq("product_spec_id", productSpecId);
            //一班总产量和百分比
            List<ProBagging> bagOne = proBaggingMapper.selectList(wrapper);
            assembleShiftData(bagOne,shiftMap,shift,shiftOne);

            EntityWrapper<ProBagging> wrapper2 = new EntityWrapper<>();
            wrapper2.eq("shift_code",shiftTwo);
            wrapper2.ge("modify_time",LocalDate.now().withDayOfYear(1)+ " 00:00:00");
            wrapper2.le("modify_time",LocalDate.now()+ " 23:59:59");
            if(rq.getFurnaceId()!=null && rq.getFurnaceId()!=0){
                wrapper2.eq("furnace_id",rq.getFurnaceId());
            }
            if(rq.getProductId()!=null && rq.getProductId()!=0){
                wrapper2.eq("product_id",rq.getProductId());
            }
            wrapper2.eq("status",EnumCommon.LogicStatus.NORMAL.getKey());
            shiftMap.put(EnumProBagging.SHIFT_AMOUNT.TWO.getValue(),shift.getShiftTwo());
            wrapper2.eq("product_spec_id", productSpecId);
            List<ProBagging> bagTwo = proBaggingMapper.selectList(wrapper2);
            assembleShiftData(bagTwo,shiftMap,shift,shiftTwo);

            //三班总产量和百分比
            EntityWrapper<ProBagging> wrapper3 = new EntityWrapper<>();
            wrapper3.eq("shift_code",shiftThree);
            wrapper3.ge("modify_time",LocalDate.now().withDayOfYear(1)+ " 00:00:00");
            wrapper3.le("modify_time",LocalDate.now()+ " 23:59:59");
            if(rq.getFurnaceId()!=null && rq.getFurnaceId()!=0){
                wrapper3.eq("furnace_id",rq.getFurnaceId());
            }
            if(rq.getProductId()!=null && rq.getProductId()!=0){
                wrapper3.eq("product_id",rq.getProductId());
            }
            wrapper3.eq("status",EnumCommon.LogicStatus.NORMAL.getKey());
            shiftMap.put(EnumProBagging.SHIFT_AMOUNT.THREE.getValue(),shift.getShiftThree());
            wrapper3.eq("product_spec_id", productSpecId);
            List<ProBagging> bagThree = proBaggingMapper.selectList(wrapper3);
            assembleShiftData(bagThree,shiftMap,shift,shiftThree);
            shiftData.add(shiftMap);
            //炉号相关
            getOutputByFurnace(shiftMap);
        }
    }

    /**
     * 获取炉号相关产量数据
     * @param shiftMap
     * @param
     */
    private void getOutputByFurnace(Map<String ,Object> shiftMap){
        //炉号产量统计
        BigDecimal amount1 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_AMOUNT.ONE.getValue());
        BigDecimal amount2 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_AMOUNT.TWO.getValue());
        BigDecimal amount3 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_AMOUNT.THREE.getValue());
        BigDecimal amountFurnace = amount1.add(amount2).add(amount3).setScale(4,BigDecimal.ROUND_HALF_UP);
        //炉号累计统计
        BigDecimal sum1 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_SUM.ONE.getValue());
        BigDecimal sum2 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_SUM.TWO.getValue());
        BigDecimal sum3 = (BigDecimal) shiftMap.get(EnumProBagging.SHIFT_SUM.THREE.getValue());
        BigDecimal sumFurnace = sum1.add(sum2).add(sum3).setScale(4,BigDecimal.ROUND_HALF_UP);
        //炉号百分比
        BigDecimal percentFurnace = amountFurnace
                .divide(sumFurnace,4,BigDecimal.ROUND_HALF_UP)
                .multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP);
        shiftMap.put(EnumProBagging.FURNACE_FIELDS.AMOUNT.getValue(),amountFurnace);
        shiftMap.put(EnumProBagging.FURNACE_FIELDS.PERCENT.getValue(),percentFurnace);
        shiftMap.put(EnumProBagging.FURNACE_FIELDS.SUM.getValue(),sumFurnace);
    }

    /**
     * 分班次获取产量相关数据
     * @param bags
     * @param map
     * @param shift
     */
    private void assembleShiftData(List<ProBagging> bags,Map<String ,Object> map,ReportFormAmountByShift shift,Integer shiftNum){

        List<ProBagging> bagging = proBaggingMapper.selectList(new EntityWrapper<ProBagging>()
                .eq("product_spec_id", shift.getProductSpecId()));
        if(!CollectionUtils.isEmpty(bagging)){
            map.put("specName",bagging.get(0).getProductSpecName());
            if(!CollectionUtils.isEmpty(bags)){
                //班次总产量和百分比
                BigDecimal shiftSum = bags.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmount()))
                        .map(ProBagging::getAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(4, BigDecimal.ROUND_HALF_UP);
                switch (shiftNum) {
                    case 1:
                        map.put(EnumProBagging.SHIFT_SUM.ONE.getValue(),shiftSum);
                        BigDecimal shiftPercent1 = shift.getShiftOne()
                                .divide(shiftSum,4,BigDecimal.ROUND_HALF_UP)
                                .multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP);
                        map.put(EnumProBagging.SHIFT_PERCENT.ONE.getValue(),shiftPercent1);
                        break;
                    case 2:
                        map.put(EnumProBagging.SHIFT_SUM.TWO.getValue(),shiftSum);
                        BigDecimal shiftPercent2 = shift.getShiftTwo()
                                .divide(shiftSum,4,BigDecimal.ROUND_HALF_UP)
                                .multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP);
                        map.put(EnumProBagging.SHIFT_PERCENT.TWO.getValue(),shiftPercent2);
                        break;
                    case 3:
                        map.put(EnumProBagging.SHIFT_SUM.THREE.getValue(),shiftSum);
                        BigDecimal shiftPercent3 = shift.getShiftThree()
                                .divide(shiftSum,4,BigDecimal.ROUND_HALF_UP)
                                .multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP);
                        map.put(EnumProBagging.SHIFT_PERCENT.THREE.getValue(),shiftPercent3);
                        break;
                    default:
                        return;
                }
            }else{
                switch (shiftNum) {
                    case 1:
                        map.put(EnumProBagging.SHIFT_SUM.ONE.getValue(),BigDecimal.ZERO);
                        map.put(EnumProBagging.SHIFT_PERCENT.ONE.getValue(),BigDecimal.ZERO);
                        break;
                    case 2:
                        map.put(EnumProBagging.SHIFT_SUM.TWO.getValue(),BigDecimal.ZERO);
                        map.put(EnumProBagging.SHIFT_PERCENT.TWO.getValue(),BigDecimal.ZERO);
                        break;
                    case 3:
                        map.put(EnumProBagging.SHIFT_SUM.THREE.getValue(),BigDecimal.ZERO);
                        map.put(EnumProBagging.SHIFT_PERCENT.THREE.getValue(),BigDecimal.ZERO);
                        break;
                    default:
                        return;
                }
            }
        }
    }



    private BigDecimal bigDecimalIsNull(BigDecimal data){
        if(ObjectUtils.isEmpty(data)){
            return  BigDecimal.ZERO;
        }
        if(data.compareTo(BigDecimal.ZERO) == 0){
            return  BigDecimal.ZERO;
        }
        return data.setScale(3,BigDecimal.ROUND_HALF_UP);
    }
}
