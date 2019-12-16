package com.bee.platform.cloud.si.manufacture.service.BigScreenStatistic.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBaggingMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBlankingDetailMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.dto.ActualPfDTO;
import com.bee.platform.cloud.si.manufacture.dto.BigScreenProductionDTO;
import com.bee.platform.cloud.si.manufacture.dto.CurrentDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ElectricityUseDTO;
import com.bee.platform.cloud.si.manufacture.dto.EnterLeaveFactoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachine;
import com.bee.platform.cloud.si.manufacture.dto.ProDeviceInspectionStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceProductionDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDeviceInspection;
import com.bee.platform.cloud.si.manufacture.entity.ESZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ProBagging;
import com.bee.platform.cloud.si.manufacture.entity.SaleWeightMachine;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterDataVO;
import com.bee.platform.cloud.si.manufacture.entity.ProDeviceInspection;
import com.bee.platform.cloud.si.manufacture.service.BigScreenStatistic.BigScreenStatisticService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceInspectionService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProBaggingService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProDeviceInspectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.cloud.si.manufacture.utils.ExcelUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.*;
import com.mysql.cj.xdevapi.Result;
import com.sun.org.apache.bcel.internal.generic.NEW;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.FieldSortBuilder;
import org.elasticsearch.search.sort.SortBuilder;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ResourceUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author xin.huang
 * @description 企业电子大屏数据展示相关接口
 * @date 2019/11/12
 */
@Slf4j
@Service
public class BigScreenStatisticServiceImpl implements BigScreenStatisticService {

    @Autowired
    private ProBaggingService baggingService;
    
    @Autowired
    private BuyWeightMachineService buyWeightMachineService;
    
    @Autowired
    private SaleWeightMachineService saleWeightMachineService;

    @Autowired
    private ProBaggingMapper baggingMapper;

    @Autowired
    private ProDeviceInspectionService deviceInspectionService;

    @Autowired
    private ConfigDeviceInspectionService configDeviceInspectionService;

    @Autowired
    private ProBlankingDetailMapper blankingDetailMapper;
    
    @Autowired
    private ElasticsearchTemplate elasticsearchTemplate;

    @Autowired
    private ConfigDeviceService configDeviceService;

    /**
     * @descriptin 查询矿热炉一周内的产量
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    @Override
    public ResponseResult<List<BigScreenProductionDTO>> findFurnaceProduction(AuthPlatformUserInfo userInfo, Integer data) {
        List<BigScreenProductionDTO> result = new ArrayList<>();
        if (data.equals(Status.TRUE.getKey())) {
            //获取截止时间
            String endTime = LocalDateUtils.formatDate(LocalDateTime.now(), LocalDateUtils.Y_M_D);
            //获取一周之前的时间
            String startTime = LocalDateUtils.getLastWeekDay(LocalDateUtils.Y_M_D);

            //真实生产数据
            Wrapper<ProBagging> baggingWrapper = new EntityWrapper<ProBagging>()
                    .eq("status", Status.TRUE.getKey())
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .ge("shift_time", startTime)
                    .le("shift_time", endTime);
            List<ProBagging> baggings = baggingService.selectList(baggingWrapper);
            if (ObjectUtils.isEmpty(baggings)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
            }
            //所有炉号总产量
            BigDecimal totalNum = baggings.stream().map(ProBagging:: getAmount)
                    .reduce(BigDecimal:: add).orElse(BigDecimal.ZERO);
            if (totalNum.compareTo(BigDecimal.ZERO) == 1) {
                Map<Integer, List<ProBagging>> baggingMap = baggings.stream()
                        .collect(Collectors.groupingBy(ProBagging :: getFurnaceId));
                for (Map.Entry<Integer, List<ProBagging>> entry : baggingMap.entrySet()) {
                    List<ProBagging> proBaggingList = entry.getValue();
                    if (ObjectUtils.isEmpty(proBaggingList)) {
                        continue;
                    }
                    //当前炉号产量
                    BigDecimal furnaceNum = proBaggingList.stream().map(ProBagging:: getAmount)
                            .reduce(BigDecimal:: add).orElse(BigDecimal.ZERO);
                    //产量占比
                    BigDecimal proportion = furnaceNum.multiply(new BigDecimal("100"))
                            .divide(totalNum, 0, BigDecimal.ROUND_HALF_UP);
                    BigScreenProductionDTO production = new BigScreenProductionDTO();
                    production.setFurnaceId(entry.getKey()).setFurnaceName(proBaggingList.get(0).getFurnaceName())
                            .setProduction(furnaceNum).setProportion(proportion);
                    result.add(production);
                }
            }
        } else {
            //模拟数据
            List<BigScreenProductionShowExcel> furnaceProductions = ExcelUtils.importExcel(getDefaultDataFile(), 0,
                    1, 0, BigScreenProductionShowExcel.class);
            result = BeanUtils.assemble(BigScreenProductionDTO.class, furnaceProductions);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

	@Override
	public ResponseResult<List<EnterLeaveFactoryDTO>> findBuyFactoryData(AuthPlatformUserInfo userInfo) {
		List<EnterLeaveFactoryDTO> result = new ArrayList<>();
		List<BuyWeightMachine> buyWeightMachines = buyWeightMachineService.selectList(new EntityWrapper<BuyWeightMachine>()
                .eq("status", Status.TRUE.getKey())
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("is_weight", Status.TRUE.getKey())
                .addFilter("DATE_FORMAT(weighing_time,'%Y-%m-%d') = {0}", LocalDate.now().toString()));
        if (ObjectUtils.isEmpty(buyWeightMachines)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        Map<Integer, List<BuyWeightMachine>> baggingMap = buyWeightMachines.stream()
                .collect(Collectors.groupingBy(BuyWeightMachine :: getProductId));
        for (Map.Entry<Integer, List<BuyWeightMachine>> entry : baggingMap.entrySet()) {
            List<BuyWeightMachine> buyWeightMachinesList = entry.getValue();
            if (ObjectUtils.isEmpty(buyWeightMachinesList)) {
                continue;
            }
            // 当前产品的出库量
            BigDecimal netWeightNum = buyWeightMachinesList.stream().map(BuyWeightMachine:: getNetWeight)
                    .reduce(BigDecimal:: add).orElse(BigDecimal.ZERO);
            EnterLeaveFactoryDTO enterLeaveFactoryDTO = new EnterLeaveFactoryDTO();
            enterLeaveFactoryDTO.setProductId(entry.getKey()).setProductName(buyWeightMachinesList.get(0).getProductName())
            					.setNetWeight(netWeightNum);
            result.add(enterLeaveFactoryDTO);
        }   
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
	}

	@Override
	public ResponseResult<List<EnterLeaveFactoryDTO>> findSaleFactoryData(AuthPlatformUserInfo userInfo) {
		List<EnterLeaveFactoryDTO> result = new ArrayList<>();
		List<SaleWeightMachine> saleWeightMachines = saleWeightMachineService.selectList(new EntityWrapper<SaleWeightMachine>()
                .eq("status", Status.TRUE.getKey())
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("is_weight", Status.TRUE.getKey())
                .addFilter("DATE_FORMAT(weighing_time,'%Y-%m-%d') = {0}", LocalDate.now().toString()));
        if (ObjectUtils.isEmpty(saleWeightMachines)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        Map<Integer, List<SaleWeightMachine>> baggingMap = saleWeightMachines.stream()
                .collect(Collectors.groupingBy(SaleWeightMachine :: getProductId));
        for (Map.Entry<Integer, List<SaleWeightMachine>> entry : baggingMap.entrySet()) {
            List<SaleWeightMachine> saleWeightMachinesList = entry.getValue();
            if (ObjectUtils.isEmpty(saleWeightMachinesList)) {
                continue;
            }
            // 当前产品的入库量
            BigDecimal netWeightNum = saleWeightMachinesList.stream().map(SaleWeightMachine:: getNetWeight)
                    .reduce(BigDecimal:: add).orElse(BigDecimal.ZERO);
            EnterLeaveFactoryDTO enterLeaveFactoryDTO = new EnterLeaveFactoryDTO();
            enterLeaveFactoryDTO.setProductId(entry.getKey()).setProductName(saleWeightMachinesList.get(0).getProductName())
            					.setNetWeight(netWeightNum);
            result.add(enterLeaveFactoryDTO);
        }   
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
	}
	
    /**
     * @descriptin 查询矿热炉一周内各成品产量
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findFurnaceFinishProduction(AuthPlatformUserInfo userInfo, Integer data) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        //存放所有矿热炉产品数据
        List<Map<String, Object>> furnaceList = new ArrayList<>();
        List<BigScreenProductionDTO> furnaceFinishProduction = null;
        if (data.equals(Status.TRUE.getKey())) {
            //获取截止时间
            String endTime = LocalDateUtils.formatDate(LocalDateTime.now(), LocalDateUtils.Y_M_D);
            //获取一周之前的时间
            String startTime = LocalDateUtils.getLastWeekDay(LocalDateUtils.Y_M_D);

            //生产数据
            Map<String, Object> params = new HashMap<>();
            params.put("startTime", startTime);
            params.put("endTime", endTime);
            params.put("enterpriseId", userInfo.getOrgId());
            if (!ObjectUtils.isEmpty(userInfo.getFactoryId())) {
                params.put("factoryId", userInfo.getFactoryId());
            }
            furnaceFinishProduction = baggingMapper.findFurnaceFinishProduction(params);
        } else {
            //模拟数据
            List<BigScreenProductionExcel> furnaceProductions = ExcelUtils.importExcel(getDefaultDataFile(), 0,
                    1, 1, BigScreenProductionExcel.class);
            furnaceFinishProduction = BeanUtils.assemble(BigScreenProductionDTO.class, furnaceProductions);
        }
        if (ObjectUtils.isEmpty(furnaceFinishProduction)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }

        //存放当前产品显示顺序
        Map<String, Integer> productOrder = new HashMap<>();
        //存放当前矿热炉下对应日期下的矿热炉数据
        Map<String, Object> furnace;
        Integer productSepcOrder = 1;
        for (BigScreenProductionDTO productionDTO : furnaceFinishProduction) {
            if (StringUtils.isNotBlank(productionDTO.getProductName())
                    && StringUtils.isNotBlank(productionDTO.getFurnaceName())) {
                String name = productionDTO.getProductName();
                //用于标识产品显示顺序
                String productSpec = productionDTO.getProductId().toString();
                if (StringUtils.isNotBlank(productionDTO.getProductSpecName())) {
                    name = name + "-" + productionDTO.getProductSpecName();
                    productSpec = productSpec + "-" + productionDTO.getProductSpecId();
                }
                if (!productOrder.containsKey(productSpec)) {
                    productOrder.put(productSpec, productSepcOrder++);
                }
                furnace = new HashMap<>();
                furnace.put("furnaceName", productionDTO.getFurnaceName());
                furnace.put("productName", name);
                furnace.put("productNum", productionDTO.getProduction());
                furnace.put("order", productOrder.get(productSpec));
                furnaceList.add(furnace);
            }
        }
        result.setFurnaces(furnaceList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 统计设备检修状况
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    @Override
    public ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(AuthPlatformUserInfo userInfo,
                                                                                         Integer data) {
        ProDeviceInspectionStatisticDTO result = new ProDeviceInspectionStatisticDTO();
        if (data.equals(Status.TRUE.getKey())) {
            //生产数据
            Wrapper<ProDeviceInspection> wrapper = new EntityWrapper<ProDeviceInspection>().eq("status", Status.TRUE.getKey())
                    .eq("complete", Status.TRUE.getKey())
                    .eq("DATE_FORMAT(create_time,'%Y-%m-%d')",
                            DateUtils.date2Format(null, DateUtils.Y_M_D, new Date()))
                    .eq("company_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId());
            List<ProDeviceInspection> deviceInspections = deviceInspectionService.selectList(wrapper);

            //总设备数
            int total = configDeviceService.selectCount(new EntityWrapper<>(new ConfigDevice()
                    .setDeleted(Status.FALSE.getKey())
                    .setEnterpriseId(userInfo.getOrgId())
                    .setFactoryId(userInfo.getFactoryId())));
            //今日待检,检修设备,正常运行,异常待修,正常运行设备
            int waitCheck = 0,checked = 0, normal=0,abnormal = 0, runNormal = 0;
            //今日巡检设备
            int currentCount = 0;
            //今日巡检率,设备运行率
            BigDecimal checkRate = BigDecimal.ZERO, runRate = BigDecimal.ZERO;
            if (!ObjectUtils.isEmpty(deviceInspections)) {
                currentCount = deviceInspections.size();

                for (ProDeviceInspection device : deviceInspections) {
                    if (device.getState().equals(1)) {
                        normal++;
                    } else if (device.getState().equals(2)) {
                        checked++;
                    } else if (device.getState().equals(3)) {
                        abnormal++;
                    }
                }
            }
            if (total > 0) {
                checkRate = new BigDecimal(currentCount).multiply(new BigDecimal(100))
                        .divide(new BigDecimal(total), 2, BigDecimal.ROUND_HALF_UP);
                runNormal = total -checked - abnormal;
                runRate = new BigDecimal(100).multiply(new BigDecimal(runNormal))
                        .divide(new BigDecimal(total), 2, BigDecimal.ROUND_HALF_UP);
            }
            result.setTotal(total).setCurrentCount(currentCount)
                    .setWaitCheck(total)
                    .setChecked(checked).setNormal(runNormal)
                    .setAbnormal(abnormal).setCheckRate(checkRate)
                    .setRunRate(runRate);
        } else {
            //模拟数据
            result.setCurrentCount(20).setWaitCheck(45).setChecked(3)
                    .setNormal(40).setAbnormal(2).setCheckRate(new BigDecimal("44.44"))
                    .setRunRate(new BigDecimal("88.89")).setTotal(45);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 查询当前时间下的原料消耗
     * @author xin.huang
     * @param userInfo
     * @date 2019/11/12
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findCurrentMaterialConsume(AuthPlatformUserInfo userInfo, Integer data) {
        List<ProFurnaceProductionDetailDTO> consumes = null;
        if (data.equals(Status.TRUE.getKey())) {
            //获取截止时间
            String endTime = LocalDateUtils.formatDate(LocalDateTime.now(), LocalDateUtils.Y_M_D_H_M_S);
            //获取一周之前的时间
            String startTime = LocalDateUtils.getLastWeekDay(LocalDateUtils.Y_M_D) + " 00:00:00";
            //生产数据
            Map<String, Object> params = new HashMap<>();
            params.put("startTime", startTime);
            params.put("endTime", endTime);
            params.put("enterpriseId", userInfo.getOrgId());
            if (!ObjectUtils.isEmpty(userInfo.getFactoryId())) {
                params.put("factoryId", userInfo.getFactoryId());
            }
            consumes = blankingDetailMapper.findCurrentMaterialConsume(params);
        } else {
            //模拟数据
            List<BigScreenMaterialConsumeExcel> consumeExcels = ExcelUtils.importExcel(getDefaultDataFile(), 0,
                    1, 2, BigScreenMaterialConsumeExcel.class);
            consumes = BeanUtils.assemble(ProFurnaceProductionDetailDTO.class, consumeExcels);

        }

        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        if (ObjectUtils.isEmpty(consumes)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }

        //存放所有矿热炉产品数据
        List<Map<String, Object>> furnaceList = new ArrayList<>();
        //存放当前炉号显示顺序
        Map<Integer, Integer> furnaceOrder = new HashMap<>();
        Integer order = 1;
        //存放当前矿热炉下对应日期下的矿热炉数据
        Map<String, Object> furnace;
        //统计各产品总量
        Map<String, BigDecimal> productTotalNum = new HashMap<>();
        consumes.forEach(consume -> {
            if (StringUtils.isNotBlank(consume.getProductName())
                    && StringUtils.isNotBlank(consume.getFurnaceName())) {
                String name = consume.getProductName();
                if (StringUtils.isNotBlank(consume.getProductSpecName())) {
                    name = name + "-" + consume.getProductSpecName();
                }
                BigDecimal productNum = consume.getAmount();
                if (productTotalNum.containsKey(name)) {
                    productNum = productNum.add(productTotalNum.get(name));
                }
                productTotalNum.put(name, productNum);
            }
        });
        for (ProFurnaceProductionDetailDTO consume : consumes) {
            if (StringUtils.isNotBlank(consume.getProductName())
                    && StringUtils.isNotBlank(consume.getFurnaceName())) {

                String name = consume.getProductName();
                if (StringUtils.isNotBlank(consume.getProductSpecName())) {
                    name = name + "-" + consume.getProductSpecName();
                }
                if (!furnaceOrder.containsKey(consume.getFurnaceId())) {
                    furnaceOrder.put(consume.getFurnaceId(), order++);
                }

                furnace = new HashMap<>();
                if (productTotalNum.containsKey(name)) {
                    furnace.put("totalNum", productTotalNum.get(name));
                    //productTotalNum.remove(name);
                }
                furnace.put("furnaceName", consume.getFurnaceName());
                furnace.put("productName", name);
                furnace.put("productNum", consume.getAmount());
                furnace.put("order", furnaceOrder.get(consume.getFurnaceId()));
                furnaceList.add(furnace);
            }
        }

        result.setFurnaces(furnaceList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }
    
    // 获取下一分钟整数的时间戳
    private Long roundMinTime(Long nowTime,int min) {
    	Calendar cal=Calendar.getInstance();
    	cal.setTimeInMillis(nowTime * 1000);
    	cal.add(Calendar.MINUTE, min);
    	Long result = cal.getTimeInMillis()/1000;
    	return result;
    }
    
    

	@Override
	public ResponseResult<List<CurrentDTO>> findElectricityByDevice(String factoryId,String deviceId,
			Long startTime,Long endTime) {
		List<CurrentDTO> result = new ArrayList<>();
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery()
        		.must(QueryBuilders.termQuery("deviceId", deviceId))
        		.must(QueryBuilders.termQuery("factoryId", factoryId))
                .must(QueryBuilders.rangeQuery("meterReadTime").gte(startTime).lte(endTime));
        Page page = new Page(ConstantsUtil.MAX_SIZE, ConstantsUtil.FRIST);
        SortBuilder sort = new FieldSortBuilder("meterReadTime").order(SortOrder.ASC);
        Pageable pageable = PageRequest.of(page.getCurrentPage() - 1, page.getPageSize());
        SearchQuery searchQuery =
                new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).withPageable(pageable).withSort(sort).build();
        List<ESZElecMeterData> eszElecMeterData = elasticsearchTemplate.queryForList(searchQuery, ESZElecMeterData.class);
		// 获取开始时间的分钟整点数
		if(! (startTime % 60 == 0) ) {
			startTime = startTime + (60 - startTime % 60);
		}
		SimpleDateFormat sdf= new SimpleDateFormat("HH:mm");
		List<Long> times = new ArrayList<>();
		for (Long time = startTime; time <= endTime;time = time + 10 * 60) {
			times.add(time);
		}
		// 处理时间格式
		// es查询到有数据，进行处理
		for (ESZElecMeterData e : eszElecMeterData) {
			Long mTime = e.getMeterReadTime();
			// 取该数据的分钟整点数
			if(! (mTime % 60 == 0) ) {
				mTime = mTime - mTime % 60;
			}
			if((mTime - startTime) % 600 == 0 ) {
				String data = sdf.format(mTime*1000);
				CurrentDTO a = new CurrentDTO();
				a.setReadTime(e.getMeterReadTime());
				a.setMeterReadTime(data);
				a.setIData(e.getIa());
				a.setItem("a");
				result.add(a);
				CurrentDTO b = new CurrentDTO();
				b.setReadTime(mTime);
				b.setMeterReadTime(data);
				b.setIData(e.getIb());
				b.setItem("b");
				result.add(b);
				CurrentDTO c = new CurrentDTO();
				c.setReadTime(mTime);
				c.setMeterReadTime(data);
				c.setIData(e.getIc());
				c.setItem("c");
				result.add(c);
				times.remove(mTime);
			}
		}
		for (Long e : times) {
			String data = sdf.format(e*1000);
			CurrentDTO a = new CurrentDTO();
			a.setReadTime(e);
			a.setMeterReadTime(data);
			a.setIData(0f);
			a.setItem("a");
			result.add(a);
			CurrentDTO b = new CurrentDTO();
			b.setReadTime(e);
			b.setMeterReadTime(data);
			b.setIData(0f);
			b.setItem("b");
			result.add(b);
			CurrentDTO c = new CurrentDTO();
			c.setReadTime(e);
			c.setMeterReadTime(data);
			c.setIData(0f);
			c.setItem("c");
			result.add(c);
		}
		// 按照时间戳排序
		Collections.sort(result, (a, b) -> a.getReadTime().compareTo(b.getReadTime()));
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
	}

	@Override
	public ResponseResult<List<ElectricityUseDTO>> findTotalElectricityUseInfo(String factoryId, Long startTime,
			Long endTime) {
		
		List<ElectricityUseDTO> result =  new ArrayList<>();
		// 处理时间格式
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
		List<ElectricityUseDTO> result1 = elecUseByDeviceId(factoryId, "Device1", startTime, endTime, sdf);
		List<ElectricityUseDTO> result2 = elecUseByDeviceId(factoryId, "Device2", startTime, endTime, sdf);
		List<ElectricityUseDTO> result3 = elecUseByDeviceId(factoryId, "Device3", startTime, endTime, sdf);
		List<ElectricityUseDTO> result4 = elecUseByDeviceId(factoryId, "Device4", startTime, endTime, sdf);
		result.addAll(result1);
		result.addAll(result2);
		result.addAll(result3);
		result.addAll(result4);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
	}

	private List<ElectricityUseDTO> elecUseByDeviceId(String factoryId, String deviceId, Long startTime, Long endTime,SimpleDateFormat sdf) {
		// 电热炉1的数据情况
		List<ElectricityUseDTO> result = new ArrayList<>();
		for (Long time = startTime; time <= endTime;time = time + 24 * 60 * 60) {
			Long time1 = time + 24 * 60 * 60;
			BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery()
	        		.must(QueryBuilders.termQuery("deviceId", deviceId))
	        		.must(QueryBuilders.termQuery("factoryId", factoryId))
	                .must(QueryBuilders.rangeQuery("meterReadTime").gte(time).lte(time1));
	        Page page = new Page(ConstantsUtil.MAX_SIZE, ConstantsUtil.FRIST);
	        SortBuilder sort = new FieldSortBuilder("meterReadTime").order(SortOrder.ASC);
	        Pageable pageable = PageRequest.of(page.getCurrentPage() - 1, page.getPageSize());
	        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).withPageable(pageable).withSort(sort).build();
	        List<ESZElecMeterData> eszElecMeterData = elasticsearchTemplate.queryForList(searchQuery, ESZElecMeterData.class);
			if(!ObjectUtils.isEmpty(eszElecMeterData)) {
				Integer size = eszElecMeterData.size();
				ElectricityUseDTO dto = new ElectricityUseDTO();
				// 这段时间 起始数据
				Float min = eszElecMeterData.get(0).getEp();
				// 这段时间 最终数据
				Float max = eszElecMeterData.get(size-1).getEp();
				dto.setDeviceId(deviceId.substring(deviceId.length() - 1));
				dto.setTime(sdf.format(new Date(time*1000)));
				dto.setPower(max - min);
				result.add(dto);
			}
			// 补0
			if(ObjectUtils.isEmpty(eszElecMeterData)) {
				ElectricityUseDTO dto = new ElectricityUseDTO();
				dto.setDeviceId(deviceId.substring(deviceId.length() - 1));
				dto.setTime(sdf.format(new Date(time*1000)));
				dto.setPower(0f);
				result.add(dto);
			}
		}
		return result;
	}

	@Override
	public List<ActualPfDTO> findActualPfc(String factoryId, Long endTime) {

		List<ActualPfDTO> result =  new ArrayList<>();
		ActualPfDTO dto1 = actualPfcByDeviceId(factoryId,"Device1", "一号炉", endTime);
		ActualPfDTO dto2 = actualPfcByDeviceId(factoryId,"Device2", "二号炉", endTime);
		ActualPfDTO dto3 = actualPfcByDeviceId(factoryId,"Device3","三号炉", endTime);
//		ActualPfDTO dto4 = actualPfcByDeviceId(factoryId,"Device4",endTime);
        if (!ObjectUtils.isEmpty(dto1)) {
            result.add(dto1);
        }
        if (!ObjectUtils.isEmpty(dto2)) {
            result.add(dto2);
        }
        if (!ObjectUtils.isEmpty(dto3)) {
            result.add(dto3);
        }
//		result.add(dto4);
		return result;
	}

	private ActualPfDTO actualPfcByDeviceId(String factoryId, String deviceId, String deviceName, Long endTime) {
		ActualPfDTO result = null;
		BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery()
        		.must(QueryBuilders.termQuery("deviceId", deviceId))
        		.must(QueryBuilders.termQuery("factoryId", factoryId))
                .must(QueryBuilders.rangeQuery("meterReadTime").lte(endTime));
		Page page = new Page(ConstantsUtil.MAX_SIZE, ConstantsUtil.FRIST);
        SortBuilder sort = new FieldSortBuilder("meterReadTime").order(SortOrder.DESC);
        Pageable pageable = PageRequest.of(page.getCurrentPage() - 1, page.getPageSize());
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).withPageable(pageable).withSort(sort).build();
        List<ESZElecMeterData> eszElecMeterData = elasticsearchTemplate.queryForList(searchQuery, ESZElecMeterData.class);
        if(!ObjectUtils.isEmpty(eszElecMeterData)) {
            result = new ActualPfDTO();
        	result.setDeviceId(deviceName);
        	result.setPf(eszElecMeterData.get(0).getPf());
        }
		return result;
	}


	/**
	 * @descriptin 获取默认数据excel文件
	 * @author xin.huang
	 * @param
	 * @date 2019/11/18
	 * @return
	 */
	private InputStream getDefaultDataFile() {
        InputStream inputStream = null;
        try {
            ClassPathResource resource = new ClassPathResource("BigScreenData.xls");
            inputStream = resource.getInputStream();
        } catch (Exception e) {
        }
        return inputStream;
    }
}
