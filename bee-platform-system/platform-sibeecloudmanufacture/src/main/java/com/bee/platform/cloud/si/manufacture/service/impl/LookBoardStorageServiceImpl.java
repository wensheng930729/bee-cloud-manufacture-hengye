package com.bee.platform.cloud.si.manufacture.service.impl;

import com.bee.platform.cloud.si.manufacture.dao.mapper.LookBoardStorageMapper;
import com.bee.platform.cloud.si.manufacture.dto.LookBoarOutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.OutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.OutStorageListDTO;
import com.bee.platform.cloud.si.manufacture.dto.RawMaterialDTO;
import com.bee.platform.cloud.si.manufacture.service.LookBoardStorageService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.utils.LocalDateUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @ClassName: LookBoardStorageServiceImpl
 * @Description: 看板库存Service
 * @Author: fei.sun
 * @Date: 2019/10/18 16:10
 * @Version: 1.0
 */
@Slf4j
@Service
public class LookBoardStorageServiceImpl implements LookBoardStorageService {

    private LookBoardStorageMapper lookBoardStorageMapper;
    /**
     * 成品类型标识
     */
    private static final Integer FINISHED_PRODUCT_TYPE = new Integer("4");
    /**
     * 按日统计标识
     */
    private static final Integer SELECT_BY_DAY = new Integer("1");

    @Autowired
    public LookBoardStorageServiceImpl(LookBoardStorageMapper lookBoardStorageMapper){
        this.lookBoardStorageMapper = lookBoardStorageMapper;
    }

    @Override
    public List<RawMaterialDTO> selectRawMaterial(Integer goodsType,String dateTime,AuthPlatformUserInfo userInfo) {
        Integer factoryId = userInfo.getFactoryId();
        Integer orgId = userInfo.getOrgId();
        //查询原料库存量
        String typeName = changeGoodsTypeToName(goodsType);
        //查询到当前时间库存量
        List<RawMaterialDTO> rawMaterialDTOS = lookBoardStorageMapper.selectRawMaterialByType(typeName,factoryId,orgId);
        LocalDate localDate = LocalDateUtils.parseDateToLocalDate(dateTime, "yyyy-MM-dd");
        if(ObjectUtils.isEmpty(localDate)){
            localDate = LocalDate.now();
        }
        LocalDate oneDayLaterDate = localDate.plusDays(1);
        LocalDate now = LocalDate.now();
        if(oneDayLaterDate.isBefore(now)&&!CollectionUtils.isEmpty(rawMaterialDTOS)){
            String startDate = LocalDateUtils.formatDate(oneDayLaterDate,"yyyy-MM-dd");
            String endDate = LocalDateUtils.formatDate(now,"yyyy-MM-dd");
            for(RawMaterialDTO rawMaterialDTO:rawMaterialDTOS){
                Integer productId = rawMaterialDTO.getProductId();
                BigDecimal productNumber = rawMaterialDTO.getProductNumber();
                //查询该产品在查询时间后一段时间的入库量
                BigDecimal inStorageProductNumber = selectInStorageProductNumber(goodsType,productId,startDate,endDate);
                //查询该产品在查询时间后一段时间的出库量
                BigDecimal outStorageProductNumber = selectOutStorageProductNumber(goodsType,productId,startDate,endDate);
                //这段时间净库存
                BigDecimal divide = inStorageProductNumber.subtract(outStorageProductNumber);
                //总库存减去净库存即为查询时间当天的库存总量
                productNumber = productNumber.subtract(divide);
                rawMaterialDTO.setProductNumber(productNumber);
            }
        }
        return rawMaterialDTOS;
    }

    @Override
    public List<LookBoarOutStorageDTO> selectOutStorage(Integer type, Integer goodsType, String startTime, String endTime
            , AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<LookBoarOutStorageDTO> lookBoarOutStorageDTOS = new ArrayList<>();
        String format;
        if(SELECT_BY_DAY.equals(type)){
            format = "%Y-%m-%d";
        }else {
            format = "%Y-%m";
        }
        String typeName = changeGoodsTypeToName(goodsType);
        Map<String,Object> params = new HashMap<>(16);
        params.put("typeName",typeName);
        params.put("startTime",startTime);
        params.put("endTime",endTime);
        params.put("orgId",orgId);
        params.put("factoryId",factoryId);
        params.put("format",format);
        //成品或原料出库量
        List<OutStorageListDTO> outStorageListDTOS;
        if(FINISHED_PRODUCT_TYPE.equals(goodsType)){
            //查询成品出库量
            outStorageListDTOS = selectFinishedProductOutNumber(params);
        }else {
            //查询原料出库量
            outStorageListDTOS = selectRawMaterialProductOutNumber(params);
        }
        //查询新增出库在这段时间内的出库量
        List<OutStorageListDTO> freeOutStorageListDTOS = selectFreeOutStorageNumberRangTime(params);
        List<OutStorageListDTO> dataList = new ArrayList<>();
        if(!CollectionUtils.isEmpty(outStorageListDTOS)){
            dataList.addAll(outStorageListDTOS);
        }
        if(!CollectionUtils.isEmpty(freeOutStorageListDTOS)){
            dataList.addAll(freeOutStorageListDTOS);
        }
        if(!CollectionUtils.isEmpty(dataList)){
            List<OutStorageListDTO> finalDataList = new ArrayList<>();
            //总出库量等于成品出库或者原料出库加上自由出库的数量
            for(OutStorageListDTO outStorageListDTO:dataList){
                boolean state = false;
                for(OutStorageListDTO finalData:finalDataList){
                    if(finalData.getProductId().equals(outStorageListDTO.getProductId())
                            &&finalData.getTime().equals(outStorageListDTO.getTime())){
                        BigDecimal productNumber = finalData.getProductNumber();
                        productNumber = productNumber.add(outStorageListDTO.getProductNumber());
                        finalData.setProductNumber(productNumber);
                        state = true;
                    }
                }
                if(!state){
                    finalDataList.add(outStorageListDTO);
                }
            }
            //转换数据格式为前端需要的格式
            Set<String> times = finalDataList.stream().map(OutStorageListDTO::getTime).collect(Collectors.toSet());
            for(String time:times){
                LookBoarOutStorageDTO lookBoarOutStorageDTO = new LookBoarOutStorageDTO();
                List<RawMaterialDTO> data = new ArrayList<>();
                for(OutStorageListDTO finalData : finalDataList){
                    if(finalData.getTime().equals(time)){
                        RawMaterialDTO rawMaterialDTO = new RawMaterialDTO();
                        rawMaterialDTO.setProductId(finalData.getProductId())
                                .setProductNumber(finalData.getProductNumber())
                                .setProductName(finalData.getProductName());
                        data.add(rawMaterialDTO);
                    }
                }
                lookBoarOutStorageDTO.setKey(time);
                lookBoarOutStorageDTO.setData(data);
                lookBoarOutStorageDTOS.add(lookBoarOutStorageDTO);
            }
        }
        return lookBoarOutStorageDTOS;
    }

    @Override
    public List<RawMaterialDTO> selectInTransit(Integer goodsType, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String typeName = changeGoodsTypeToName(goodsType);
        return lookBoardStorageMapper.selectInTransit(typeName,orgId,factoryId);
    }

    /**
     * 查询新增出库量根据时间和产品分组
     * @param params 参数集合
     * @return a
     */
    private List<OutStorageListDTO> selectFreeOutStorageNumberRangTime(Map<String, Object> params) {
        return lookBoardStorageMapper.selectFreeOutStorageNumberRangTime(params);
    }

    /**
     * 查询原料出库量
     * @param params a
     * @return b
     */
    private List<OutStorageListDTO> selectRawMaterialProductOutNumber(Map<String,Object> params) {
        return lookBoardStorageMapper.selectRawMaterialProductOutNumber(params);
    }


    /**
     * 查询成品出库数量
     * @param params a
     * @return b
     */
    private List<OutStorageListDTO> selectFinishedProductOutNumber(Map<String,Object> params) {
        return lookBoardStorageMapper.selectFinishedProductOutNumber(params);
    }

    /**
     *  查询该产品在查询时间后一段时间的出库量
     * @param goodsType a
     * @param productId b
     * @param startDate c
     * @param endDate d
     * @return e
     */
    private BigDecimal selectOutStorageProductNumber(Integer goodsType,Integer productId, String startDate, String endDate) {
        BigDecimal productNumber;
        if(FINISHED_PRODUCT_TYPE.equals(goodsType)){
            //成品出库量
            productNumber = lookBoardStorageMapper.selectFinishedProductOutStorageByRangTime(productId,startDate,endDate);
        }else {
            //原料出库量
            productNumber = lookBoardStorageMapper.selectRawMaterialsProductOutStorageByRangTime(productId,startDate,endDate);
        }
        //新增出库量
        BigDecimal freeOutStorageNumber = lookBoardStorageMapper.selectFreeOutStorageNumberByRangtime(productId,startDate,endDate);
        return freeOutStorageNumber.add(productNumber);
    }

    /**
     * 查询该产品在查询时间后一段时间的入库量
     * @param goodsType c
     * @param productId d
     * @param startDate d
     * @param endDate f
     * @return e
     */
    private BigDecimal selectInStorageProductNumber(Integer goodsType,Integer productId, String startDate, String endDate) {
        BigDecimal productNumber;
        if(FINISHED_PRODUCT_TYPE.equals(goodsType)){
            //产成品入库量
            productNumber = lookBoardStorageMapper.selectFinishedProductInStorageByRangTime(productId,startDate,endDate);
        }else {
            //原料入库量
            productNumber = lookBoardStorageMapper.selectRawMaterialInStorageByRangTime(productId,startDate,endDate);
        }
        //自由入库量
        BigDecimal freeInStorageNumber = lookBoardStorageMapper.selectFreeInStorageNumberByRangtime(productId,startDate,endDate);
        return productNumber.add(freeInStorageNumber);
    }

    /**
     * 根据类型标识转换成类型名称
     * @param goodsType 类型标识
     * @return 类型名称
     */
    private String changeGoodsTypeToName(Integer goodsType){
        String typeName ;
        switch (goodsType){
            case 1:
                typeName = "主料";
                break;
            case 2:
                typeName = "辅料";
                break;
            case 3:
                typeName = "其他";
                break;
            default:
                typeName = "成品";
        }
        return typeName;
    }
}
