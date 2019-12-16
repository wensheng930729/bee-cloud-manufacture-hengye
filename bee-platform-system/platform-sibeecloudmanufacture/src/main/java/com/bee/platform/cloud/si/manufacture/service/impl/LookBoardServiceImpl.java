package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumLookBoardType;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.LookBoardRQ;
import com.bee.platform.cloud.si.manufacture.rq.PurchasePaymentRQ;
import com.bee.platform.cloud.si.manufacture.service.LookBoardService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class LookBoardServiceImpl implements LookBoardService {

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;
    @Autowired
    private BuyContractBasicService buyContractBasicService;
    @Autowired
    private BuyWeightMachineService buyWeightMachineService;
    @Autowired
    private SaleContractBasicService saleContractBasicService;
    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;
    @Autowired
    private SaleWeightMachineService saleWeightMachineService;
    @Autowired
    private ProBaggingMapper proBaggingMapper;
    @Autowired
    private FinishedProductBeOutOfStorageMapper finishedProductBeOutOfStorageMapper;
    @Autowired
    private FinishedProductOutStorageDetailMapper finishedProductOutStorageDetailMapper;
    @Autowired
    private SaleCarrierTransportDetailService saleCarrierTransportDetailService;

    @Override
    public ResponseResult<List<UnfinishedFinanceDTO>> getBuyUnfinishedFinance(AuthPlatformUserInfo userInfo, Integer type) {
        List<UnfinishedFinanceDTO> contractBasic;
        HashMap<String, Object> map = new HashMap<>();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factory_id", userInfo.getFactoryId());
        map.put("categoryId", type);
        // 根据业务线先查询
        if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            contractBasic = buyContractBasicMapper.getSupplierUnfinishedFinance(map);
        } else {
            contractBasic = buyContractBasicMapper.getUnfinishedFinance(map);
        }
        contractBasic = contractBasic.stream().filter(a ->
                a.getAlready().compareTo(BigDecimal.ZERO) > 0
                        || a.getExpect().compareTo(BigDecimal.ZERO) > 0
                        || a.getShould().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBasic);
    }

    private HashMap<String, Object> getParamMap(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        Integer type = rq.getType();
        HashMap<String, Object> map = Maps.newHashMap();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("startTime", rq.getStartTime());
        map.put("endTime", rq.getEndTime());
        if (EnumLookBoardType.ParamType.MAIN.getKey().equals(type)) {
            map.put("categoryId", EnumLookBoardType.ProductCategory.MAIN.getKey());
        } else if (EnumLookBoardType.ParamType.ACCESSORIE.getKey().equals(type)) {
            map.put("categoryId", EnumLookBoardType.ProductCategory.ACCESSORIE.getKey());
        } else if (EnumLookBoardType.ParamType.PRODUCT.getKey().equals(type)) {
            map.put("categoryId", EnumLookBoardType.ProductCategory.PRODUCT.getKey());
        } else if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            map.put("categoryId", null);
        }
        return map;
    }

    @Override
    public ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchaseMoneyRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> paramMap = getParamMap(rq, userInfo);
        List<BuyPurchaseMoneyRatioDTO> dtoList = buyContractBasicMapper.getPurchaseMoney(paramMap);
        dtoList = dtoList.stream().filter(a -> a.getMoney().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        // 如果查询类型不是 客户  则以产品为统计对象
        if (!Objects.equals(EnumLookBoardType.ParamType.CUSTOMER.getKey(), rq.getType()) && dtoList.size() > 1) {
            HashMap<Integer, BuyPurchaseMoneyRatioDTO> map = Maps.newHashMap();
            for (BuyPurchaseMoneyRatioDTO dto : dtoList) {
                Integer productId = dto.getProductId();
                BigDecimal money = dto.getMoney();
                BuyPurchaseMoneyRatioDTO resultDto = map.get(productId);
                if (resultDto == null) {
                    BuyPurchaseMoneyRatioDTO dto1 = new BuyPurchaseMoneyRatioDTO().setProductId(productId).setProductName(dto.getProductName());
                    map.put(productId, money == null ? dto1.setMoney(BigDecimal.ZERO) : dto1.setMoney(money));
                } else {
                    resultDto.setMoney(resultDto.getMoney().add(money == null ? BigDecimal.ZERO : money));
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(map.values()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<List<BuyPurchaseAmountRatioDTO>> getPurchaseAmountRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> paramMap = getParamMap(rq, userInfo);
        List<BuyPurchaseAmountRatioDTO> dtoList = buyContractBasicMapper.getPurchaseAmount(paramMap);
        dtoList = dtoList.stream().filter(a -> a.getAmount().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        // 如果查询类型不是 客户  则以产品为统计对象
        if (!Objects.equals(EnumLookBoardType.ParamType.CUSTOMER.getKey(), rq.getType()) && dtoList.size() > 1) {
            HashMap<Integer, BuyPurchaseAmountRatioDTO> map = Maps.newHashMap();
            for (BuyPurchaseAmountRatioDTO dto : dtoList) {
                Integer productId = dto.getProductId();
                BigDecimal money = dto.getAmount();
                BuyPurchaseAmountRatioDTO resultDto = map.get(productId);
                if (resultDto == null) {
                    BuyPurchaseAmountRatioDTO dto1 = new BuyPurchaseAmountRatioDTO().setProductId(productId).setProductName(dto.getProductName());
                    map.put(productId, money == null ? dto1.setAmount(BigDecimal.ZERO) : dto1.setAmount(money));
                } else {
                    resultDto.setAmount(resultDto.getAmount().add(money == null ? BigDecimal.ZERO : money));
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(map.values()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchasePaymentRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> paramMap = getParamMap(rq, userInfo);
        List<BuyPurchaseMoneyRatioDTO> dtoList = buyContractBasicMapper.getPurchasePayment(paramMap);
        dtoList = dtoList.stream().filter(a -> a.getMoney().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        // 如果查询类型不是 客户  则以产品为统计对象
        if (!Objects.equals(EnumLookBoardType.ParamType.CUSTOMER.getKey(), rq.getType()) && dtoList.size() > 1) {
            HashMap<Integer, BuyPurchaseMoneyRatioDTO> map = Maps.newHashMap();
            for (BuyPurchaseMoneyRatioDTO dto : dtoList) {
                Integer productId = dto.getProductId();
                BigDecimal money = dto.getMoney();
                BuyPurchaseMoneyRatioDTO resultDto = map.get(productId);
                if (resultDto == null) {
                    BuyPurchaseMoneyRatioDTO dto1 = new BuyPurchaseMoneyRatioDTO().setProductId(productId).setProductName(dto.getProductName());
                    map.put(productId, money == null ? dto1.setMoney(BigDecimal.ZERO) : dto1.setMoney(money));
                } else {
                    resultDto.setMoney(resultDto.getMoney().add(money == null ? BigDecimal.ZERO : money));
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(map.values()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<List<BuyPurchasePassRatioDTO>> getPurchasePassRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        Integer type = rq.getType();
        if (EnumLookBoardType.ParamType.MAIN.getKey().equals(type)) {
            return getPassRatioPurchase(1, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.ACCESSORIE.getKey().equals(type)) {
            return getPassRatioPurchase(2, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.PRODUCT.getKey().equals(type)) {
            return getPassRatioPurchase(3, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            return getPassRatioPurchase(null, rq, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
    }

    private ResponseResult<List<BuyPurchasePassRatioDTO>> getPassRatioPurchase(Integer categoryId, PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        List<BuyContractBasic> contractBasicList = buyContractBasicService.selectList(new EntityWrapper<>(new BuyContractBasic()
                .setStatus(Status.TRUE.getKey())
                .setCategoryId(categoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId()))
                .between("sign_date", rq.getStartTime() + " 00:00:00", rq.getEndTime() + " 23:59:59"));
        List<BuyPurchasePassRatioDTO> dtoList = calculatePurchasePassRatio(contractBasicList, rq.getType());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    // 计算合格率
    private List<BuyPurchasePassRatioDTO> calculatePurchasePassRatio(List<BuyContractBasic> contractBasicList, Integer type) {
        Set<String> businessIdList = contractBasicList.stream().map(BuyContractBasic::getContractBusinessId).collect(Collectors.toSet());
        List<BuyWeightMachine> machineList = buyWeightMachineService.selectList(new EntityWrapper<>(new BuyWeightMachine()
                .setStatus(Status.TRUE.getKey()))
                .in("contract_business_id", businessIdList)
                // 只查询质检结果出来的
                .isNotNull("assay_result"));
        Map<String, BigDecimal> machineTotalMap = new HashMap<>();
        Map<String, BigDecimal> machinePassMap = new HashMap<>();
        for (BuyWeightMachine machine : machineList) {
            String contractBusinessId = machine.getContractBusinessId();
            BigDecimal total = machineTotalMap.get(contractBusinessId);
            BigDecimal pass = machinePassMap.get(contractBusinessId);
            // 每个磅单的净重求和 合同业务id为key，净重和为value
            if (total == null) {
                machineTotalMap.put(contractBusinessId, machine.getNetWeight() == null ? BigDecimal.ZERO : machine.getNetWeight());
            } else {
                if (machine.getNetWeight() != null) {
                    machineTotalMap.put(contractBusinessId, total.add(machine.getNetWeight()));
                }
            }
            // 如果磅单合格 净重加入machinePassMap
            if (Objects.equals(machine.getAssayResult(), 1)) {
                if (pass == null) {
                    machinePassMap.put(contractBusinessId, machine.getNetWeight() == null ? BigDecimal.ZERO : machine.getNetWeight());
                } else {
                    if (machine.getNetWeight() != null) {
                        machinePassMap.put(contractBusinessId, pass.add(machine.getNetWeight()));
                    }
                }
            }
        }
        Map<Integer, BigDecimal> totalMap = new HashMap<>();
        Map<Integer, BigDecimal> passMap = new HashMap<>();
        Map<Integer, BuyPurchasePassRatioDTO> resultMap = new HashMap<>();
        Integer keyId;
        for (BuyContractBasic contractBasic : contractBasicList) {
            if (Objects.equals(type, EnumLookBoardType.ParamType.CUSTOMER.getKey())) {
                keyId = contractBasic.getSupplierId();
            } else {
                keyId = contractBasic.getProductId();
            }
            String contractBusinessId = contractBasic.getContractBusinessId();

            BigDecimal total = totalMap.get(keyId);
            BigDecimal pass = passMap.get(keyId);
            // 每个供应商/产品所有的净重
            if (total == null) {
                totalMap.put(keyId, machineTotalMap.get(contractBusinessId) == null ? BigDecimal.ZERO : machineTotalMap.get(contractBusinessId));
            } else {
                if (machineTotalMap.get(contractBusinessId) != null) {
                    totalMap.put(keyId, total.add(machineTotalMap.get(contractBusinessId)));
                }
            }
            //   每个供应商/产品合格的净重
            if (pass == null) {
                passMap.put(keyId, machinePassMap.get(contractBusinessId) == null
                        ? BigDecimal.ZERO : machinePassMap.get(contractBusinessId));
            } else {
                if (machinePassMap.get(contractBusinessId) != null) {
                    passMap.put(keyId, pass.add(machinePassMap.get(contractBusinessId)));
                }
            }
            // resultMap 供应商id为key，返回dto：BuyPurchasePassRatioDTO为value
            BuyPurchasePassRatioDTO dto = resultMap.get(keyId);
            if (dto == null) {
                resultMap.put(keyId, new BuyPurchasePassRatioDTO()
                        .setSupplierId(keyId)
                        .setSupplierName(contractBasic.getSupplierName())
                        .setProductId(contractBasic.getProductId())
                        .setProductName(contractBasic.getProductName()));
            }
        }
        // 计算合格率
        List<Integer> delIdList = Lists.newArrayList();
        for (Integer id : resultMap.keySet()) {
            // 如果总数为0 则返回0
            BigDecimal total = totalMap.get(id);
            BigDecimal pass = passMap.get(id);
            if (total.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal passRatio = pass.divide(total, 4, RoundingMode.HALF_UP)
                        .multiply(BigDecimal.valueOf(100));
                resultMap.put(id, resultMap.get(id)
                        .setPassRatio(passRatio)
                        .setFailureRatio(BigDecimal.valueOf(100)
                                .subtract(passRatio)
                                .setScale(4, RoundingMode.HALF_UP)));
            } else {
                delIdList.add(id);
            }
        }
        for (Integer delId : delIdList) {
            resultMap.remove(delId);
        }
        return new ArrayList<>(resultMap.values());
    }

    @Override
    public ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> paramMap = getParamMap(rq, userInfo);
        List<SaleMoneyRatioDTO> dtoList = saleContractBasicMapper.getSaleMoney(paramMap);
        dtoList = dtoList.stream().filter(a -> a.getMoney().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        // 如果查询类型不是 客户  则以产品为统计对象
        if (!Objects.equals(EnumLookBoardType.ParamType.CUSTOMER.getKey(), rq.getType()) && dtoList.size() > 1) {
            HashMap<Integer, SaleMoneyRatioDTO> map = Maps.newHashMap();
            for (SaleMoneyRatioDTO dto : dtoList) {
                Integer productId = dto.getProductId();
                BigDecimal money = dto.getMoney();
                SaleMoneyRatioDTO resultDto = map.get(productId);
                if (resultDto == null) {
                    SaleMoneyRatioDTO dto1 = new SaleMoneyRatioDTO().setProductId(productId).setProductName(dto.getProductName());
                    map.put(productId, money == null ? dto1.setMoney(BigDecimal.ZERO) : dto1.setMoney(money));
                } else {
                    resultDto.setMoney(resultDto.getMoney().add(money == null ? BigDecimal.ZERO : money));
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(map.values()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<List<SaleAmountRatioDTO>> getSaleAmountRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        Integer type = rq.getType();
        SaleContractBasic contractBasic = new SaleContractBasic()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setStatus(Status.TRUE.getKey());
        if (!EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            contractBasic.setCategoryId(type);
        }
        HashMap<Integer, SaleAmountRatioDTO> resultMap = Maps.newHashMap();
        // 合同
        List<SaleContractBasic> contractBasicList = saleContractBasicService.selectList(new EntityWrapper<>(contractBasic)
                .between("sign_date", rq.getStartTime(), rq.getEndTime()));
        Set<String> contractNumList = contractBasicList.stream().map(SaleContractBasic::getContractNum).collect(Collectors.toSet());

        // 查询合同对应车辆
        List<FinishedProductBeOutOfStorage> carList;
        if (CollectionUtils.isEmpty(contractNumList)) {
            carList = new ArrayList<>(0);
        } else {
            carList = finishedProductBeOutOfStorageMapper.selectList(new EntityWrapper<FinishedProductBeOutOfStorage>()
                    .eq("status", Status.TRUE.getKey())
                    .in("contract_id", contractNumList));
        }
        Set<String> contractCarIdList = carList.stream().map(FinishedProductBeOutOfStorage::getContractCarId).collect(Collectors.toSet());
        // 查询车辆对应吨袋
        List<FinishedProductOutStorageDetail> tonList;
        if (CollectionUtils.isEmpty(contractCarIdList)) {
            tonList = new ArrayList<>(0);
        } else {
            tonList = finishedProductOutStorageDetailMapper.selectList(new EntityWrapper<FinishedProductOutStorageDetail>()
                    .in("contract_car_id", contractCarIdList));

        }
        // 统计数量
        getSaleAmountResultMap(type, contractBasicList, resultMap, carList, tonList);

        Collection<SaleAmountRatioDTO> values = resultMap.values();
        if (values.size() == 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<SaleAmountRatioDTO> dtoList = new ArrayList<>(values);
        dtoList = dtoList.stream().filter(a -> (a.getAmount().compareTo(BigDecimal.ZERO) > 0
                || a.getEndAmount().compareTo(BigDecimal.ZERO) > 0)).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    /**
     * @param type              查询的类型
     * @param contractBasicList 合同列表
     * @param resultMap         结果map
     * @param carList           车辆列表
     * @param tonList           吨袋列表
     */
    private void getSaleAmountResultMap(Integer type, List<SaleContractBasic> contractBasicList, HashMap<Integer, SaleAmountRatioDTO> resultMap, List<FinishedProductBeOutOfStorage> carList, List<FinishedProductOutStorageDetail> tonList) {
        // 车辆对应吨袋重量
        HashMap<String, BigDecimal> tonMap = Maps.newHashMap();
        for (FinishedProductOutStorageDetail ton : tonList) {
            BigDecimal val = tonMap.get(ton.getContractCarId());
            if (val == null) {
                tonMap.put(ton.getContractCarId(), ton.getProductNumber() == null ? BigDecimal.ZERO : ton.getProductNumber());
            } else {
                if (ton.getProductNumber() != null) {
                    tonMap.put(ton.getContractCarId(), val.add(
                            ton.getProductNumber() == null ? BigDecimal.ZERO : ton.getProductNumber()));
                }
            }
        }
        // 合同对应车辆重量
        HashMap<String, BigDecimal> carMap = Maps.newHashMap();
        for (FinishedProductBeOutOfStorage car : carList) {
            BigDecimal val = carMap.get(car.getContractId());
            if (val == null) {
                carMap.put(car.getContractId(),
                        tonMap.get(car.getContractCarId()) == null ? BigDecimal.ZERO : tonMap.get(car.getContractCarId()));
            } else {
                if (tonMap.get(car.getContractCarId()) != null) {
                    carMap.put(car.getContractId(), val.add(tonMap.get(car.getContractCarId())));
                }
            }
        }
        // 如果是查询客户的信息
        if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            for (SaleContractBasic contract : contractBasicList) {
                SaleAmountRatioDTO val = resultMap.get(contract.getCustomerId());
                if (val == null) {
                    resultMap.put(contract.getCustomerId(), new SaleAmountRatioDTO()
                            .setCustomerId(contract.getCustomerId())
                            .setCustomerName(contract.getCustomerName())
                            .setProductId(contract.getProductId())
                            .setProductName(contract.getProductName())
                            // 客户销售总数量
                            .setAmount(contract.getQuantity())
                            // 客户销售产成品数量
                            .setEndAmount(
                                    carMap.get(contract.getContractNum()) == null
                                            ? BigDecimal.ZERO
                                            : carMap.get(contract.getContractNum())));
                } else {
                    if (carMap.get(contract.getContractNum()) != null) {
                        val.setEndAmount(val.getEndAmount().add(carMap.get(contract.getContractNum())));
                    }
                    val.setAmount(val.getAmount().add(contract.getQuantity() == null ? BigDecimal.ZERO : contract.getQuantity()));
                }
            }
        } else {
            for (SaleContractBasic contract : contractBasicList) {
                SaleAmountRatioDTO val = resultMap.get(contract.getProductId());
                if (val == null) {
                    resultMap.put(contract.getProductId(), new SaleAmountRatioDTO()
                            .setCustomerId(contract.getCustomerId())
                            .setCustomerName(contract.getCustomerName())
                            .setProductId(contract.getProductId())
                            .setProductName(contract.getProductName())
                            // 客户销售总数量
                            .setAmount(contract.getQuantity())
                            // 客户销售产成品数量
                            .setEndAmount(
                                    carMap.get(contract.getContractNum()) == null
                                            ? BigDecimal.ZERO
                                            : carMap.get(contract.getContractNum())));
                } else {
                    if (carMap.get(contract.getContractNum()) != null) {
                        val.setEndAmount(val.getEndAmount().add(carMap.get(contract.getContractNum())));
                    }
                    val.setAmount(val.getAmount().add(contract.getQuantity() == null ? BigDecimal.ZERO : contract.getQuantity()));
                }
            }
        }
    }

    @Override
    public ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyBackRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        HashMap<String, Object> paramMap = getParamMap(rq, userInfo);
        List<SaleMoneyRatioDTO> dtoList = saleContractBasicMapper.getSaleMoneyBack(paramMap);
        dtoList = dtoList.stream().filter(a -> a.getMoney().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        // 如果查询类型不是 客户  则以产品为统计对象
        if (!Objects.equals(EnumLookBoardType.ParamType.CUSTOMER.getKey(), rq.getType()) && dtoList.size() > 1) {
            HashMap<Integer, SaleMoneyRatioDTO> map = Maps.newHashMap();
            for (SaleMoneyRatioDTO dto : dtoList) {
                Integer productId = dto.getProductId();
                BigDecimal money = dto.getMoney();
                SaleMoneyRatioDTO resultDto = map.get(productId);
                if (resultDto == null) {
                    SaleMoneyRatioDTO dto1 = new SaleMoneyRatioDTO().setProductId(productId).setProductName(dto.getProductName());
                    map.put(productId, money == null ? dto1.setMoney(BigDecimal.ZERO) : dto1.setMoney(money));
                } else {
                    resultDto.setMoney(resultDto.getMoney().add(money == null ? BigDecimal.ZERO : money));
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(map.values()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    @Override
    public ResponseResult<List<SalePassRatioDTO>> getSalePassRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        Integer type = rq.getType();
        if (EnumLookBoardType.ParamType.MAIN.getKey().equals(type)) {
            return getPassRatioSale(1, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.ACCESSORIE.getKey().equals(type)) {
            return getPassRatioSale(2, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.PRODUCT.getKey().equals(type)) {
            return getPassRatioSale(3, rq, userInfo);
        } else if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            return getPassRatioSale(null, rq, userInfo);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
    }

    private ResponseResult<List<SalePassRatioDTO>> getPassRatioSale(Integer categoryId, PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo) {
        List<SaleContractBasic> contractBasicList = saleContractBasicService.selectList(new EntityWrapper<>(new SaleContractBasic()
                .setStatus(Status.TRUE.getKey())
                .setCategoryId(categoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId()))
                .between("sign_date", rq.getStartTime() + " 00:00:00", rq.getEndTime() + " 23:59:59"));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, calculateSalePassRatio(contractBasicList, rq.getType()));
    }

    /**
     * 计算合格率
     *
     * @param contractBasicList 符合条件的合同list
     * @param type
     * @return Map  key：客户id/产品id value：SalePassRatioDTO
     */
    private List<SalePassRatioDTO> calculateSalePassRatio(List<SaleContractBasic> contractBasicList, @NotNull Integer type) {
        Set<String> businessIdList = contractBasicList.stream().map(SaleContractBasic::getContractBusinessId).collect(Collectors.toSet());
        List<SaleCarrierTransportDetail> carrierTransportDetailList = saleCarrierTransportDetailService.selectList(new EntityWrapper<>(new SaleCarrierTransportDetail()
                .setStatus(Status.TRUE.getKey())
                // 到厂的重量
                .setArrivalStatus(Status.TRUE.getKey()))
                .in("contract_business_id", businessIdList));
        Map<String, BigDecimal> carrierTotalMap = new HashMap<>();
        Map<String, BigDecimal> carrierPassMap = new HashMap<>();
        for (SaleCarrierTransportDetail carrier : carrierTransportDetailList) {
            String contractBusinessId = carrier.getContractBusinessId();
            BigDecimal total = carrierTotalMap.get(contractBusinessId);
            BigDecimal pass = carrierPassMap.get(contractBusinessId);
            // 每个磅单的净重求和 合同业务id为key，净重和为value
            if (total == null) {
                carrierTotalMap.put(contractBusinessId, carrier.getCargoWeight() == null ? BigDecimal.ZERO : carrier.getCargoWeight());
            } else {
                if (carrier.getCargoWeight() != null) {
                    carrierTotalMap.put(contractBusinessId, total.add(carrier.getCargoWeight()));
                }
            }
            // 如果合格 重量加入carrierPassMap
            if (Objects.equals(carrier.getAssayResult(), Status.TRUE.getKey())) {
                if (pass == null) {
                    carrierPassMap.put(contractBusinessId, carrier.getCargoWeight() == null
                            ? BigDecimal.ZERO : carrier.getCargoWeight());
                } else {
                    if (carrier.getCargoWeight() != null) {
                        carrierPassMap.put(contractBusinessId, pass.add(carrier.getCargoWeight()));
                    }
                }
            }
        }
        Map<Integer, BigDecimal> totalMap = new HashMap<>();
        Map<Integer, BigDecimal> passMap = new HashMap<>();
        Map<Integer, SalePassRatioDTO> resultMap = new HashMap<>();
        Integer keyId;
        for (SaleContractBasic contractBasic : contractBasicList) {
            if (Objects.equals(type, EnumLookBoardType.ParamType.CUSTOMER.getKey())) {
                keyId = contractBasic.getCustomerId();
            } else {
                keyId = contractBasic.getProductId();
            }
            String contractBusinessId = contractBasic.getContractBusinessId();

            BigDecimal total = totalMap.get(keyId);
            BigDecimal pass = passMap.get(keyId);
            // 每个客户/产品所有的净重
            if (total == null) {
                totalMap.put(keyId, carrierTotalMap.get(contractBusinessId) == null ? BigDecimal.ZERO : carrierTotalMap.get(contractBusinessId));
            } else {
                if (carrierTotalMap.get(contractBusinessId) != null) {
                    totalMap.put(keyId, totalMap.get(keyId).add(carrierTotalMap.get(contractBusinessId)));
                }
            }
            //  每个客户/产品合格的净重
            if (pass == null) {
                passMap.put(keyId, carrierPassMap.get(contractBusinessId) == null ? BigDecimal.ZERO : carrierPassMap.get(contractBusinessId));
            } else {
                if (carrierPassMap.get(contractBusinessId) != null) {
                    passMap.put(keyId, pass.add(carrierPassMap.get(contractBusinessId)));
                }
            }
            // resultMap 客户id为key，返回dto：SalePassRatioDTO 为value
            SalePassRatioDTO dto = resultMap.get(keyId);
            if (dto == null) {
                resultMap.put(keyId, new SalePassRatioDTO()
                        .setCustomerId(keyId)
                        .setCustomerName(contractBasic.getCustomerName())
                        .setProductId(contractBasic.getProductId())
                        .setProductName(contractBasic.getProductName()));
            }
        }
        // 计算合格率
        List<Integer> delIdList = Lists.newArrayList();
        for (Integer id : resultMap.keySet()) {
            // 如果总数为0 则返回0
            BigDecimal total = totalMap.get(id);
            BigDecimal pass = passMap.get(id);
            if (total.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal passRatio = pass.divide(total, 4, RoundingMode.HALF_UP)
                        .multiply(BigDecimal.valueOf(100));
                resultMap.put(id, resultMap.get(id)
                        .setPassRatio(passRatio)
                        .setFailureRatio(BigDecimal.valueOf(100)
                                .subtract(passRatio)
                                .setScale(4, RoundingMode.HALF_UP)));
            } else {
                delIdList.add(id);
            }
        }
        for (Integer delId : delIdList) {
            resultMap.remove(delId);
        }
        return new ArrayList<>(resultMap.values());
    }

    @Override
    public ResponseResult<List<UnfinishedFinanceDTO>> getSaleUnfinishedFinance(AuthPlatformUserInfo userInfo,
                                                                               Integer type) {
        List<UnfinishedFinanceDTO> contractBasic;
        HashMap<String, Object> map = new HashMap<String, Object>();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factory_id", userInfo.getFactoryId());
        map.put("categoryId", type);
        // 根据业务线先查询
        if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(type)) {
            contractBasic = saleContractBasicMapper.getSupplierUnfinishedFinance(map);
        } else {
            contractBasic = saleContractBasicMapper.getUnfinishedFinance(map);
        }
        contractBasic = contractBasic.stream().filter(a ->
                a.getAlready().compareTo(BigDecimal.ZERO) > 0
                        || a.getExpect().compareTo(BigDecimal.ZERO) > 0
                        || a.getShould().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBasic);
    }

    @Override
    public ResponseResult<List<UnfinishedGoodsDTO>> getBuyUnfinishedGoods(LookBoardRQ rq,
                                                                          AuthPlatformUserInfo userInfo) {
        List<UnfinishedGoodsDTO> contractBasic;
        HashMap<String, Object> map = Maps.newHashMap();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("startTime", rq.getStartTime());
        map.put("endTime", rq.getEndTime());
        map.put("categoryId", rq.getType());
        if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(rq.getType())) {
            contractBasic = buyContractBasicMapper.getSupplierUnfinishedGoods(map);
        } else {
            contractBasic = buyContractBasicMapper.getUnfinishedGoods(map);
        }
        contractBasic = contractBasic.stream().filter(a ->
                a.getAlready().compareTo(BigDecimal.ZERO) > 0
                        || a.getIncomplete().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBasic);
    }

    @Override
    public ResponseResult<List<UnfinishedGoodsDTO>> getSaleUnfinishedGoods(LookBoardRQ rq,
                                                                           AuthPlatformUserInfo userInfo) {
        List<UnfinishedGoodsDTO> contractBasic;
        HashMap<String, Object> map = Maps.newHashMap();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("startTime", rq.getStartTime());
        map.put("endTime", rq.getEndTime());
        map.put("categoryId", rq.getType());
        if (EnumLookBoardType.ParamType.CUSTOMER.getKey().equals(rq.getType())) {
            contractBasic = saleContractBasicMapper.getSupplierUnfinishedGoods(map);
        } else {
            contractBasic = saleContractBasicMapper.getUnfinishedGoods(map);
        }
        contractBasic = contractBasic.stream().filter(a ->
                a.getAlready().compareTo(BigDecimal.ZERO) > 0
                        || a.getIncomplete().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBasic);
    }

    /**
     * 查询BI数据总览
     */
    @Override
    public ResponseResult<DataScreenDTO> getDataScreen(AuthPlatformUserInfo userInfo) {
        LocalDate now = LocalDate.now();
        int year = now.getYear();

        DataScreenDTO dto = new DataScreenDTO();
        HashMap<String, Object> map = new HashMap<>();
        map.put("enterpriseId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("paramDate", now);

        BigDecimal buyDay = buyContractBasicMapper.getDataScreen(map);
        BigDecimal saleDay = saleContractBasicMapper.getDataScreen(map);
        BigDecimal proDay = proBaggingMapper.getDataScreen(map);

        map.put("paramDate", year);
        BigDecimal buyYear = buyContractBasicMapper.getDataScreen(map);
        BigDecimal saleYear = saleContractBasicMapper.getDataScreen(map);
        BigDecimal proYear = proBaggingMapper.getDataScreen(map);

        dto.setBuyDay(buyDay == null ? BigDecimal.ZERO : buyDay.setScale(3, RoundingMode.HALF_UP));
        dto.setSaleDay(saleDay == null ? BigDecimal.ZERO : saleDay.setScale(3, RoundingMode.HALF_UP));
        dto.setProDay(proDay == null ? BigDecimal.ZERO : proDay.setScale(3, RoundingMode.HALF_UP));

        dto.setBuyYear(buyYear == null ? BigDecimal.ZERO : buyYear.setScale(3, RoundingMode.HALF_UP));
        dto.setSaleYear(saleYear == null ? BigDecimal.ZERO : saleYear.setScale(3, RoundingMode.HALF_UP));
        dto.setProYear(proYear == null ? BigDecimal.ZERO : proYear.setScale(3, RoundingMode.HALF_UP));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}
