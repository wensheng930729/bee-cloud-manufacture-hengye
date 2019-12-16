package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumProduct;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProArtificialFeedMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBaggingMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBlankingDetailMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProOreFurnaceRecordDetailMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceInspectionService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author xin.huang
 * @description 生产数据统计
 * @date 2019/10/18
 */
@Slf4j
@Service
public class ProStatisticServiceImpl implements ProStatisticService {

    @Autowired
    private ProBlankingService blankingService;

    @Autowired
    private ProBaggingService baggingService;

    @Autowired
    private ProMaterialBatchService materialBatchService;

    @Autowired
    private ProBaggingMapper baggingMapper;

    @Autowired
    private ProBlankingDetailMapper blankingDetailMapper;

    @Autowired
    private ProOreFurnaceRecordDetailMapper oreFurnaceRecordDetailMapper;

    @Autowired
    private ProDeviceInspectionService deviceInspectionService;

    @Autowired
    private ConfigDeviceInspectionService configDeviceInspectionService;

    @Autowired
    private ProArtificialFeedService artificialFeedService;

    @Autowired
    private ProArtificialFeedMapper artificialFeedMapper;

    /**
     * @descriptin 条件查询矿热炉回收率
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/18
     * @return
     */
    @Override
    public ResponseResult<List<ProFurnaceRateDTO>> findFurnaceRecoveryRate(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        List<ProFurnaceRateDTO> result = new ArrayList<>();
        //plc下料
        Wrapper<ProBlanking> blankingWrapper = new EntityWrapper<ProBlanking>()
                .eq("status", Status.TRUE.getKey())
                .eq("company_id", userInfo.getOrgId());
        //人工补料
        Wrapper<ProArtificialFeed> feedWrapper = new EntityWrapper<ProArtificialFeed>()
                .eq("status", Status.TRUE.getKey())
                .eq("company_id", userInfo.getOrgId())
                .eq("type", Status.FALSE.getKey());
        //成品装袋
        Wrapper<ProBagging> baggingWrapper = new EntityWrapper<ProBagging>()
                .eq("status", Status.TRUE.getKey())
                .eq("enterprise_id", userInfo.getOrgId());
        if (Objects.nonNull(userInfo.getFactoryId())) {
            baggingWrapper.eq("factory_id", userInfo.getFactoryId());
            blankingWrapper.eq("factory_id", userInfo.getFactoryId());
            feedWrapper.eq("factory_id", userInfo.getFactoryId());
        }
        if (!ObjectUtils.isEmpty(rq.getProductId())) {
            baggingWrapper.eq("product_id", rq.getProductId());
            blankingWrapper.eq("finish_product_id", rq.getProductId());
            feedWrapper.eq("finish_product_id", rq.getProductId());
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            blankingWrapper.ge("blanking_time", rq.getStartTime() + " 00:00:00");
            baggingWrapper.ge("shift_time", rq.getStartTime() + " 00:00:00");
            feedWrapper.ge("blanking_time", rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            blankingWrapper.le("blanking_time", rq.getEndTime() + " 23:59:59");
            baggingWrapper.le("shift_time", rq.getEndTime() + " 23:59:59");
            feedWrapper.le("blanking_time", rq.getEndTime() + " 23:59:59");
        }

        //下料记录
        List<ProBlanking> blankings = blankingService.selectList(blankingWrapper);

        //人工补料
        List<ProArtificialFeed> feeds = artificialFeedService.selectList(feedWrapper);

        //成品装袋记录
        List<ProBagging> baggings = baggingService.selectList(baggingWrapper);

        if (!ObjectUtils.isEmpty(blankings) && !ObjectUtils.isEmpty(baggings)) {
            Map<Integer, List<ProBlanking>> blankingMap = blankings.stream()
                    .collect(Collectors.groupingBy(ProBlanking :: getFurnaceId));
            Map<Integer, List<ProBagging>> baggingMap = baggings.stream()
                    .collect(Collectors.groupingBy(ProBagging :: getFurnaceId));
            Map<Integer, List<ProArtificialFeed>> feedMap = new HashMap<>();
            if (!ObjectUtils.isEmpty(feeds)) {
                feedMap = feeds.stream().collect(Collectors.groupingBy(ProArtificialFeed :: getFurnaceId));
            }
            for (Map.Entry<Integer, List<ProBagging>> entry : baggingMap.entrySet()) {
                if (blankingMap.containsKey(entry.getKey())) {
                    //当前炉号对应的下料中的主料数量
                    BigDecimal totalMainNum = blankingMap.get(entry.getKey())
                            .stream().map(ProBlanking :: getMainNum)
                            .reduce(BigDecimal :: add).orElse(BigDecimal.ZERO);
                    if (feedMap.containsKey(entry.getKey())) {
                        BigDecimal feedNum = feedMap.get(entry.getKey()).stream()
                                .map(ProArtificialFeed :: getNum)
                                .reduce(BigDecimal :: add).orElse(BigDecimal.ZERO);
                        totalMainNum = totalMainNum.add(feedNum);
                    }
                    //当前炉号对应的成品数量
                    BigDecimal totalFinish = entry.getValue().stream()
                            .map(ProBagging :: getAmount)
                            .reduce(BigDecimal :: add)
                            .orElse(BigDecimal.ZERO);
                    if (totalMainNum.compareTo(BigDecimal.ZERO) == 1) {
                        BigDecimal recoveryRate = totalFinish.multiply(new BigDecimal(EnumProduct.MAIN_CONTENT.FINISH.getKey()))
                                .divide(totalMainNum.multiply(new BigDecimal(EnumProduct
                                        .MAIN_CONTENT.INGREDIENT.getKey())), 2, BigDecimal.ROUND_HALF_UP)
                                .multiply(new BigDecimal("100"));
                        ProFurnaceRateDTO furnaceRate = new ProFurnaceRateDTO();
                        furnaceRate.setFurnaceId(entry.getKey())
                                .setFurnaceName(entry.getValue().get(0).getFurnaceName())
                                .setRecoveryRate(recoveryRate);
                        result.add(furnaceRate);
                    }
                }
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 条件查询矿热炉产量
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/21
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findFurnaceProduction(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }
        List<ProFurnaceProductionDetailDTO> details = baggingMapper.findProduction(rq);
        if (ObjectUtils.isEmpty(details)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        //存放对应矿热炉及相关统计数据
        Map<String, Map<String, Object>> furnaceMap = new HashMap<>();
        BigDecimal totalNum = BigDecimal.ZERO;
        for (ProFurnaceProductionDetailDTO detail : details) {
            if (Objects.nonNull(detail.getAmount())) {
                totalNum = totalNum.add(detail.getAmount());
            }
            if (StringUtils.isNotBlank(detail.getFurnaceName())
                    && StringUtils.isNotBlank(detail.getCurrentDate())) {

                String currentDate = detail.getCurrentDate();

                //存放当前矿热炉下对应日期下的矿热炉数据
                Map<String, Object> furnace;
                if (furnaceMap.containsKey(detail.getFurnaceName())) {
                    furnace = furnaceMap.get(detail.getFurnaceName());
                    furnace.put(currentDate, detail.getAmount());
                } else {
                    furnace = new HashMap<>();
                    furnace.put("name", detail.getFurnaceName());
                    furnace.put(currentDate, detail.getAmount());
                    furnaceMap.put(detail.getFurnaceName(), furnace);
                }
            }
        }
        result.setTotalNum(totalNum);
        if (!ObjectUtils.isEmpty(furnaceMap)) {
            result.setFurnaces(furnaceMap.values());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 条件查询矿热炉产出质量
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findProductSpecs(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }
        List<ProFurnaceProductionDetailDTO> productSpecs = baggingMapper.findProductSpecs(rq);
        if (ObjectUtils.isEmpty(productSpecs)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }

        //存放对应矿热炉及相关统计数据
        Map<String, Map<String, Object>> furnaceMap = new HashMap<>();
        productSpecs.forEach(detail -> {
            if (StringUtils.isNotBlank(detail.getProductSpecName())
                    && StringUtils.isNotBlank(detail.getCurrentDate())) {
                String currentDate = detail.getCurrentDate();
                //存放当前矿热炉下对应日期下的矿热炉数据
                Map<String, Object> furnace;
                if (furnaceMap.containsKey(detail.getProductSpecName())) {
                    furnace = furnaceMap.get(detail.getProductSpecName());
                    furnace.put(currentDate, detail.getAmount());
                } else {
                    furnace = new HashMap<>();
                    furnace.put("name", detail.getProductSpecName());
                    furnace.put(currentDate, detail.getAmount());
                    furnaceMap.put(detail.getProductSpecName(), furnace);
                }
            }
        });
        if (!ObjectUtils.isEmpty(furnaceMap)) {
            result.setFurnaces(furnaceMap.values());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 条件查询原料消耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findMaterialConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }

        List<ProFurnaceProductionDetailDTO> materialConsumes = blankingDetailMapper.findMaterialConsume(rq);
        if (ObjectUtils.isEmpty(materialConsumes)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }

        BigDecimal totalNum = BigDecimal.ZERO;
        //人工补料
        List<ProArtificialFeedDTO> feedconsumes = artificialFeedMapper.findConsume(rq);
        Map<String, List<ProArtificialFeedDTO>> feedMap = new HashMap<>();
        if (!ObjectUtils.isEmpty(feedconsumes)) {
            feedMap = feedconsumes.stream().collect(Collectors.groupingBy(ProArtificialFeedDTO :: getCurrentDate));
            totalNum = feedconsumes.stream().map(ProArtificialFeedDTO :: getNum)
                    .reduce(BigDecimal :: add).orElse(BigDecimal.ZERO);
        }
        //存放对应矿热炉及相关统计数据
        Map<String, Map<String, Object>> furnaceMap = new HashMap<>();
        for (ProFurnaceProductionDetailDTO detail : materialConsumes) {
            if (Objects.nonNull(detail.getAmount())) {
                totalNum = totalNum.add(detail.getAmount());
            }
            if (StringUtils.isNotBlank(detail.getFurnaceName())
                    && StringUtils.isNotBlank(detail.getCurrentDate())) {
                String currentDate = detail.getCurrentDate();
                BigDecimal amount = detail.getAmount();
                if (feedMap.containsKey(currentDate)) {
                    List<ProArtificialFeedDTO> artificialFeeds = feedMap.get(currentDate);
                    for (ProArtificialFeedDTO feed : artificialFeeds) {
                        if (detail.getFurnaceId().equals(feed.getFurnaceId())) {
                            amount = amount.add(feed.getNum());
                            break;
                        }
                    }
                }
                //存放当前矿热炉下对应日期下的矿热炉数据
                Map<String, Object> furnace;
                if (furnaceMap.containsKey(detail.getFurnaceName())) {
                    furnace = furnaceMap.get(detail.getFurnaceName());
                    furnace.put(currentDate, amount);
                } else {
                    furnace = new HashMap<>();
                    furnace.put("name", detail.getFurnaceName());
                    furnace.put(currentDate, amount);
                    furnaceMap.put(detail.getFurnaceName(), furnace);
                }
            }
        }
        result.setTotalNum(totalNum);
        if (!ObjectUtils.isEmpty(furnaceMap)) {
            result.setFurnaces(furnaceMap.values());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 条件查询主料/辅料吨耗
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findMaterialTonConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }

        //总产量
        ProFurnaceProductionDetailDTO totalProduction = baggingMapper.findTotalProduction(rq);
        BigDecimal totalAmount = BigDecimal.ZERO;
        if (!ObjectUtils.isEmpty(totalProduction) && !ObjectUtils.isEmpty(totalProduction.getAmount())) {
            totalAmount = totalProduction.getAmount();
        }

        //主料或辅料消耗
        List<ProFurnaceProductionDetailDTO> materials= blankingDetailMapper.findMaterialMainOrAuxiliary(rq);
        if (ObjectUtils.isEmpty(materials)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        //人工补料中原料消耗
        List<ProArtificialFeedDTO> mainOrAuxiliarys = artificialFeedMapper.findMainOrAuxiliary(rq);
        Map<String, BigDecimal> feedNum = new HashMap<>();
        if (!ObjectUtils.isEmpty(mainOrAuxiliarys)) {
            mainOrAuxiliarys.forEach(feed -> {
                feedNum.put(feed.getProductId() + "-" + feed.getProductSpecId(), feed.getNum());
            });
        }
        Map<String, Object> furnaceDetailMap = new HashMap<>();
        for (ProFurnaceProductionDetailDTO material : materials) {
            BigDecimal tonConsume = BigDecimal.ZERO;
            BigDecimal amount = material.getAmount();
            if (!ObjectUtils.isEmpty(amount) && totalAmount.compareTo(BigDecimal.ZERO) == 1) {
                if (feedNum.containsKey(material.getProductId() + "-" + material.getProductSpecId())) {
                    amount = amount.add(feedNum.get(material.getProductId() + "-" + material.getProductSpecId()));
                }
                tonConsume = amount.divide(totalAmount, 2, BigDecimal.ROUND_HALF_UP);
            }
            String productName = material.getProductName();
            if (StringUtils.isBlank(productName)) {
                continue;
            }
            if (StringUtils.isNotBlank(material.getProductSpecName())) {
                productName += "-" + material.getProductSpecName();
            }
            furnaceDetailMap.put(productName, tonConsume);
        }
        List<Map<String, Object>> materialConsumes = new ArrayList<>(1);
        materialConsumes.add(furnaceDetailMap);
        result.setFurnaces(materialConsumes);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 条件查询矿热炉电力消耗
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findPowerConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }
        List<ProFurnacePowerConsumeDTO> powerConsumes = oreFurnaceRecordDetailMapper.findPowerConsume(rq);
        if (ObjectUtils.isEmpty(powerConsumes)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        //存放对应矿热炉及相关统计数据
        Map<String, Map<String, Object>> furnaceMap = new HashMap<>();
        BigDecimal totalNum = BigDecimal.ZERO;
        for (ProFurnacePowerConsumeDTO detail : powerConsumes) {
            if (Objects.nonNull(detail.getPowerConsume())) {
                totalNum = totalNum.add(detail.getPowerConsume());
            }
            if (StringUtils.isNotBlank(detail.getFurnaceName())
                    && StringUtils.isNotBlank(detail.getCurrentDate())) {
                String currentDate = detail.getCurrentDate();
                //存放当前矿热炉下对应日期下的矿热炉数据
                Map<String, Object> furnace;
                if (furnaceMap.containsKey(detail.getFurnaceName())) {
                    furnace = furnaceMap.get(detail.getFurnaceName());
                    furnace.put(currentDate, detail.getPowerConsume());
                } else {
                    furnace = new HashMap<>();
                    furnace.put("name", detail.getFurnaceName());
                    furnace.put(currentDate, detail.getPowerConsume());
                    furnaceMap.put(detail.getFurnaceName(), furnace);
                }
            }
        }
        result.setTotalNum(totalNum);
        if (!ObjectUtils.isEmpty(furnaceMap)) {
            result.setFurnaces(furnaceMap.values());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 查询当天的吨电耗
     * @author xin.huang
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    @Override
    public ResponseResult<ProFurnaceDataDTO> findPowerTonConsume(ProStatisticRQ rq, AuthPlatformUserInfo userInfo) {
        ProFurnaceDataDTO result = new ProFurnaceDataDTO();
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            rq.setStartTime(rq.getStartTime() + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            rq.setEndTime(rq.getEndTime() + " 23:59:59");
        }
        //获取当天每个炉号的产量
        List<ProFurnaceProductionDetailDTO> production = baggingMapper.findFurnaceProduction(rq);
        Map<Integer, ProFurnaceProductionDetailDTO> productionMap = new HashMap<>();
        if (!ObjectUtils.isEmpty(production)) {
            productionMap = production.stream().collect(Collectors
                    .toMap(ProFurnaceProductionDetailDTO :: getFurnaceId, Function.identity()));
        }

        //获取当天每个炉号的总电力消耗
        List<ProFurnacePowerConsumeDTO> powerConsumes = oreFurnaceRecordDetailMapper.findTotalPowerConsume(rq);
        if (ObjectUtils.isEmpty(powerConsumes)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
        }
        Map<String, Object> furnace = new HashMap<>();
        for (ProFurnacePowerConsumeDTO powerConsume : powerConsumes) {
            if (StringUtils.isBlank(powerConsume.getFurnaceName())) {
                continue;
            }
            //当前炉号的电力消耗
            BigDecimal pc = BigDecimal.ZERO;
            if (productionMap.containsKey(powerConsume.getFurnaceId())) {
                BigDecimal amount = productionMap.get(powerConsume.getFurnaceId()).getAmount();
                if (amount.compareTo(BigDecimal.ZERO) == 1) {
                    pc = powerConsume.getPowerConsume().divide(amount, 2, BigDecimal.ROUND_HALF_UP);
                }
            }
            furnace.put(powerConsume.getFurnaceName(), pc);
        }
        List<Map<String, Object>> tonConsumes = new ArrayList<>(1);
        tonConsumes.add(furnace);
        result.setFurnaces(tonConsumes);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @descriptin 统计设备检修状况
     * @author xin.huang
     * @param userInfo
     * @date 2019/10/22
     * @return
     */
    public ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(AuthPlatformUserInfo userInfo) {
        ProDeviceInspectionStatisticDTO result = new ProDeviceInspectionStatisticDTO();
        Wrapper<ProDeviceInspection> wrapper = new EntityWrapper<ProDeviceInspection>().eq("status", Status.TRUE.getKey())
                .eq("complete", Status.TRUE.getKey())
                .eq("DATE_FORMAT(create_time,'%Y-%m-%d')",
                        DateUtils.date2Format(null, DateUtils.Y_M_D, new Date()))
                .eq("company_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId());
        List<ProDeviceInspection> deviceInspections = deviceInspectionService.selectList(wrapper);

        if (!ObjectUtils.isEmpty(deviceInspections)) {
            //今日待检,检修设备,正常运行,异常待修
            int waitCheck = 0,checked = 0, normal=0,abnormal = 0;
            //今日巡检设备
            int currentCount = deviceInspections.size();
            //总设备数
            int total = configDeviceInspectionService.selectCount(new EntityWrapper<>(new ConfigDeviceInspection()
                    .setDeleted(Status.FALSE.getKey())
                    .setEnterpriseId(userInfo.getOrgId())
                    .setFactoryId(userInfo.getFactoryId())));
            for (ProDeviceInspection device : deviceInspections) {
                if (device.getState().equals(1)) {
                    normal++;
                } else if (device.getState().equals(2)) {
                    checked++;
                } else if (device.getState().equals(3)) {
                    abnormal++;
                }
            }
            //今日巡检率,设备运行率
            BigDecimal checkRate = BigDecimal.ZERO, runRate = BigDecimal.ZERO;
            if (total > 0) {
                checkRate = new BigDecimal(currentCount).multiply(new BigDecimal(100))
                        .divide(new BigDecimal(total), 2, BigDecimal.ROUND_HALF_UP);
                runRate = new BigDecimal(checked).add(new BigDecimal(normal))
                        .divide(new BigDecimal(total), 2, BigDecimal.ROUND_HALF_UP)
                        .multiply(new BigDecimal(100));
            }

            result.setCurrentCount(currentCount).setWaitCheck(waitCheck)
                    .setChecked(checked).setNormal(normal)
                    .setAbnormal(abnormal).setCheckRate(checkRate)
                    .setRunRate(runRate);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }
}
