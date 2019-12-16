package com.bee.platform.cloud.si.manufacture.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumInventoryType;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StockInventoryDetailMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StockInventoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.InventoryRQ;
import com.bee.platform.cloud.si.manufacture.rq.InventorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.StockInventoryDetailRQ;
import com.bee.platform.cloud.si.manufacture.rq.StockInventoryRQ;
import com.bee.platform.cloud.si.manufacture.service.StockInventoryService;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductCategoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductSpecService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 库存盘点主表 服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
@Slf4j
@Service
public class StockInventoryServiceImpl extends ServiceImpl<StockInventoryMapper, StockInventory>
        implements StockInventoryService {

    @Autowired
    private ConfigProductCategoryService productCategoryService;

    @Autowired
    private ConfigProductService configProductService;

    @Autowired
    private ConfigRepositoryService configRepositoryService;

    @Autowired
    private ConfigProductSpecService configProductSpecService;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private StockInventoryMapper stockInventoryMapper;

    @Autowired
    private StorageInventoryService storageInventoryService;

    @Autowired
    private StockInventoryDetailMapper stockInventoryDetailMapper;
    /**
     * @notes: 盘点单类型查询
     * @Author: junyang.li
     * @Date: 17:06 2019/11/25
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryTypeDTO>>
     */
    @Override
    public ResponseResult<List<InventoryTypeDTO>> getInventoryType() {
        EnumInventoryType[]  types=EnumInventoryType.values();
        List<InventoryTypeDTO> list=new ArrayList<>();
        for (EnumInventoryType item:types) {
            list.add(new InventoryTypeDTO(item.getCode(),item.getDesc()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * @notes: 根据盘点分类获得下拉列表详细
     * @Author: junyang.li
     * @Date: 9:55 2019/11/26
     * @param inventoryTypeCode : 分类类型
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO>>
     */
    @Override
    public ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDesc(AuthPlatformUserInfo userInfo,
                                                                           Integer inventoryTypeCode) {
        EnumInventoryType type = EnumInventoryType.getInventoryTypeByCode(inventoryTypeCode);
        if(type==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.INVENTORY_TYPE_NOT_FUND);
        }
        //遍历
        switch (type){
            case ALL:return this.getInventoryTypeDescByAll();
            case PRODUCT_TYPE:return this.getInventoryTypeDescByProductType(userInfo);
            case PRODUCT:return this.getInventoryTypeDescByProduct(userInfo);
            case WAREHOUSE:return this.getInventoryTypeDescByWarehouse(userInfo);
            default:return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
    }
    /**
     * @notes:  创建库存盘点单，并返回待盘点数据
     * @Author: junyang.li
     * @Date: 13:31 2019/11/26
     * @param userInfo : 当前操作人
     * @param rq : 待查询的类别参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    @Override
    public ResponseResult<InventoryOrderDTO> createInventoryOrder(AuthPlatformUserInfo userInfo, InventoryRQ rq) {
        EnumInventoryType type = EnumInventoryType.getInventoryTypeByCode(rq.getInventoryTypeCode());
        if(type==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.INVENTORY_TYPE_NOT_FUND);
        }
        //遍历
        switch (type){
            case ALL:return this.createInventoryByAll(userInfo);
            case PRODUCT_TYPE:return this.createInventoryByProductType(userInfo,rq.getCategoryId());
            case PRODUCT:return this.createInventoryByProduct(userInfo,rq.getCategoryId());
            case WAREHOUSE:return this.createInventoryByWarehouse(userInfo,rq.getCategoryId());
            default:return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
    }
    /**
     * @notes: 保存盘点单调整后的数据
     * @Author: junyang.li
     * @Date: 9:26 2019/11/27
     * @param userInfo : 当前用户
     * @param rq : 调整后数据
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveInventoryInfo(AuthPlatformUserInfo userInfo, StockInventoryRQ rq) {
        //根据盘点单id从缓存中获取盘点单信息
        String inventoryOrderId=rq.getInventoryOrderId();
        InventoryOrderDTO dto=jedisService.getObject(inventoryOrderId,InventoryOrderDTO.class);
        if(dto==null){
            log.info("无法通过盘点单号:{}从缓存中获取盘点单具体信息",inventoryOrderId);
            return ResponseResult.buildResponseResult(ResCodeEnum.INVENTORY_ORDER_NOT_FOUND);
        }
        Integer factoryId=userInfo.getFactoryId();
        //非当前工厂盘点单
        if(!factoryId.equals(dto.getFactoryId())){
            log.info("非当前工厂盘点单,没有权限修改，当前用户工厂id是:{},盘点单的工厂id是:{},盘点单单编号是:{}",
                    userInfo.getFactoryId(),dto.getFactoryId(),inventoryOrderId);
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        //获得的实盘数量
        List<StockInventoryDetailRQ> inventoryDetail=rq.getInventoryDetail();
        //将产品id + 规格id + 仓库id 最为map的键，实盘数量作为值，方便定位都某条数据
        Map<String, BigDecimal> map=new HashMap<>(32);
        inventoryDetail.forEach(obj->{
            String key=obj.getProductId()+""+obj.getProductSpecId()+""+obj.getStorageId();
            BigDecimal actualNum=BigDecimalUtils.isNull(obj.getActualNum());
            obj.setActualNum(actualNum);
            map.put(key,actualNum);
        });
        //遍历盘点单的详细信息
        List<InventoryProductDetailDTO> productDetail=dto.getList();
        Integer status = Status.TRUE.getKey();
        Integer createId=userInfo.getId();
        String  creator=userInfo.getName();
        Date createTime=new Date();
        //将待更新storage_inventory表的数据放入map中，键是库存表的逻辑id，值是待更新的货物数量
        List<StorageInventory> updated=new ArrayList<>(32);
        List<StockInventoryDetail> list=productDetail.stream().map(obj->{
            String key=obj.getProductId()+""+obj.getProductSpecId()+""+obj.getStorageId();
            BigDecimal actualNum=map.get(key);
            StockInventoryDetail detail= BeanUtils.copyProperties(obj,StockInventoryDetail.class)
                    .setCreateId(createId)
                    .setCreateTime(createTime)
                    .setCreator(creator)
                    .setInventoryOrderId(inventoryOrderId)
                    .setStatus(status);
            //计算差异数量
            if(actualNum!=null){
                detail.setActualNum(actualNum)
                        .setDifferenceNum(BigDecimalUtils.subtract(obj.getAccountNum(),actualNum));
                StorageInventory inventory=new StorageInventory(obj.getStorageInventoryId(),
                        actualNum,createId,creator,LocalDateTime.now());
                updated.add(inventory);
            }
            return detail;
        }).collect(Collectors.toList());
        //盘点单主表数据
        StockInventory stockInventory=new StockInventory(inventoryOrderId,rq.getInventoryName(),dto.getInventoryType(),
                factoryId,userInfo.getOrgId(),rq.getRemarks(),status,status,userInfo.getId(),
                userInfo.getName(),new Date());
        //将数据插入到数据库中
        this.insert(stockInventory);
        stockInventoryDetailMapper.insertAll(list);
        //更新库存中的实际数量
        storageInventoryService.updateStorageInventories(updated);
        //删除盘点单对应的缓存
        jedisService.delKey(inventoryOrderId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 盘点单列表查询
     * @Author: junyang.li
     * @Date: 14:33 2019/11/27
     * @param userInfo : 当前操作人
     * @param rq : 查询参数
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.StockInventoryListDTO>>
     */
    @Override
    public ResponseResult<List<StockInventoryListDTO>> getInventoryInfoList(AuthPlatformUserInfo userInfo,
                                                                            InventorySearchRQ rq) {
        //查询开始时间
        Date startTime=DateUtils.stringToDate(rq.getStartTime());
        //查询结束时间
        Date endTime=DateUtils.stringToDate(rq.getEndTime());
        //page对象
        Page page=rq.getPage();
        if(page==null){
            //默认每页20条 ，第一页
            page=new Page(20,1);
        }
        Pagination pagination= PageUtils.transFromPage(page);
        //从数据中查询
        StockInventoryListParam param=new StockInventoryListParam(userInfo.getOrgId(),
                userInfo.getFactoryId(),startTime,endTime);
        List<StockInventory> list=stockInventoryMapper.getInventoryInfoList(pagination,param);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list.stream().map(obj->{
            return new StockInventoryListDTO(obj.getInventoryOrderId(),obj.getCreateTime());
        }).collect(Collectors.toList()),PageUtils.transToPage(pagination));
    }
    /**
     * @notes: 根据盘点单号查询盘点单详细
     * @Author: junyang.li
     * @Date: 15:31 2019/11/27
     * @param userInfo : 当前操作人
     * @param inventoryOrderId : 盘点单号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO>
     */
    @Override
    public ResponseResult<InventoryOrderDTO> getInventoryInfoById(AuthPlatformUserInfo userInfo, String inventoryOrderId) {
        Integer factoryId=userInfo.getFactoryId();
        //查询该盘点单是否存在
        StockInventory inventory=this.selectOne(new EntityWrapper<StockInventory>().where("status =1 and immutable=1 ")
                .and("inventory_order_id={0}",inventoryOrderId)
                .and("enterprise_id={0}",userInfo.getOrgId())
                .and("factory_id={0}",factoryId));
        //判空
        if(inventory==null){
            log.info("无法通过盘点单号查询到盘点单详细，盘点单号是:{}",inventoryOrderId);
            return ResponseResult.buildResponseResult(ResCodeEnum.INVENTORY_ORDER_NOT_FOUND);
        }
        //查询盘点单详细
        List<StockInventoryDetail> details=stockInventoryDetailMapper.selectList(new EntityWrapper<StockInventoryDetail>()
                .where("status =1 and inventory_order_id={0}",inventoryOrderId));
        //遍历获得规格id
        List<Integer> productSpecIds=details.stream().map(StockInventoryDetail::getProductSpecId).collect(Collectors.toList());
        //查询规格和产品的详细信息
        List<ConfigProductSpec> specs=configProductSpecService.getProductSpecByProductIds(new ProductSpecParam().setFactoryId(factoryId)
                .setProductSpecIds(productSpecIds));
        //遍历查询的产品规格信息，并放入map中方便获取
        Map<Integer,ConfigProductSpec> map=new HashMap<>(32);
        specs.forEach(obj->map.put(obj.getId(),obj));
        //查询该工厂下的所有仓库
        List<ConfigRepository> repositories=configRepositoryService.getRepositoryList(userInfo);
        //遍历获得工厂id对应的名称
        Map<Integer,String> repositoriesMap=new HashMap<>(32);
        repositories.forEach(obj->repositoriesMap.put(obj.getId(),obj.getName()));
        //组装数据
        InventoryOrderDTO dto=BeanUtils.copyProperties(inventory,InventoryOrderDTO.class);
        List<InventoryProductDetailDTO> list=details.stream().map(obj->{
            ConfigProductSpec productSpec=map.get(obj.getProductSpecId());
            InventoryProductDetailDTO detail=BeanUtils.copyProperties(obj,InventoryProductDetailDTO.class)
                    .setStorageName(repositoriesMap.get(obj.getStorageId()));
            if(productSpec!=null){
                detail.setProductSpecName(productSpec.getSpecName())
                        .setProductName(productSpec.getProductName());
            }
            return detail;
        }).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto.setList(list));
    }

    /**
     * @notes: 盘点类型为全盘，创建盘点单
     * @Author: junyang.li
     * @Date: 13:40 2019/11/26
     * @param userInfo :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    private ResponseResult<InventoryOrderDTO> createInventoryByAll(AuthPlatformUserInfo userInfo){
        Integer factoryId=userInfo.getFactoryId();
        //查询所有品类的产品
        List<ConfigProductSpec> list=configProductSpecService.getProductSpecByProductIds(new ProductSpecParam()
                .setFactoryId(factoryId));
        //判空
        if(CollectionUtils.isEmpty(list)){
            log.info("盘点类型为全盘未查询到产品和规格信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_ALL);
        }
        //创建订单id
        InventoryOrderDTO orderDTO=new InventoryOrderDTO()
                .setInventoryOrderId(this.createBusinessId())
                .setFactoryId(factoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setInventoryType(EnumInventoryType.ALL.getCode());
        return this.getInventoryInfoByProductSpec(list,orderDTO);
    }
    /**
     * @notes: 盘点类型为按产品分类盘点，创建盘点单
     * @Author: junyang.li
     * @Date: 13:49 2019/11/26
     * @param userInfo : 当前操作人
     * @param categoryId : 产品分类参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    private ResponseResult<InventoryOrderDTO> createInventoryByProductType(AuthPlatformUserInfo userInfo,
                                                                             List<Integer> categoryId){
        Integer factoryId=userInfo.getFactoryId();
        //判空，未勾选产品分类
        if(CollectionUtils.isEmpty(categoryId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SELECTION_PRODUCT_TYPE);
        }
        ProductSpecParam param=new ProductSpecParam().setFactoryId(factoryId)
                .setCategoryIds(categoryId);
        //查询所有品类的产品
        List<ConfigProductSpec> list=configProductSpecService.getProductSpecByProductIds(param);
        //判空
        if(CollectionUtils.isEmpty(list)){
            log.info("盘点类型为按产品分类盘点未查询到产品和规格信息，查询参数是:{}",param);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FUND_PRODUCT_BY_TYPE);
        }
        //创建订单id
        InventoryOrderDTO orderDTO=new InventoryOrderDTO()
                .setInventoryOrderId(this.createBusinessId())
                .setFactoryId(factoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setInventoryType(EnumInventoryType.PRODUCT_TYPE.getCode());
        return this.getInventoryInfoByProductSpec(list,orderDTO);
    }
    /**
     * @notes: 盘点类型为按产品盘点，创建盘点单
     * @Author: junyang.li
     * @Date: 13:49 2019/11/26
     * @param userInfo : 当前操作人
     * @param categoryId : 产品id参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    private ResponseResult<InventoryOrderDTO> createInventoryByProduct(AuthPlatformUserInfo userInfo,
                                                                         List<Integer> categoryId){
        Integer factoryId=userInfo.getFactoryId();
        //判空，未勾选产品
        if(CollectionUtils.isEmpty(categoryId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SELECTION_PRODUCT);
        }
        ProductSpecParam param=new ProductSpecParam().setFactoryId(factoryId)
                .setProductIds(categoryId);
        //查询所有品类的产品
        List<ConfigProductSpec> list=configProductSpecService.getProductSpecByProductIds(param);
        //判空
        if(CollectionUtils.isEmpty(list)){
            log.info("盘点类型为按产品盘点未查询到产品和规格信息，查询参数是:{}",param);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FUND_PRODUCT_BY_ID);
        }
        //创建订单id
        InventoryOrderDTO orderDTO=new InventoryOrderDTO()
                .setInventoryOrderId(this.createBusinessId())
                .setFactoryId(factoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setInventoryType(EnumInventoryType.PRODUCT.getCode());
        return this.getInventoryInfoByProductSpec(list,orderDTO);
    }
    /**
     * @notes: 将 全部盘点,产品类别盘点,产品盘点等方法的公共方法整合到一起  (第一次拆分)
     * @Author: junyang.li
     * @Date: 16:25 2019/11/26
     * @param list : 查询出来的产品规格及产品信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO>
     */
    private ResponseResult<InventoryOrderDTO> getInventoryInfoByProductSpec(List<ConfigProductSpec> list,
                                                                            InventoryOrderDTO orderDTO){
        //初始化产品规格对应的产品容器
        Map<Integer,ConfigProductSpec> map=new HashMap<>(64);
        List<Integer> productSpecIds=new ArrayList<>(64);
        //不为空 获得所有的规格id
        list.forEach(obj->{
            Integer  productSpecId =obj.getId();
            productSpecIds.add(productSpecId);
            map.put(productSpecId,obj);
        });
        //根据规格id查询对应的仓库及产品数量
        List<StorageInventory>  storage=storageInventoryService.getStorageByProductSpecIds(orderDTO.getFactoryId(),productSpecIds);
        return this.assembleData(orderDTO,map,storage);
    }
    /**
     * @notes: 组装数据，适用于全部盘点,产品类别盘点,产品盘点,库存盘点等方法 （第二次拆分）
     * @Author: junyang.li
     * @Date: 17:19 2019/11/26
     * @param orderDTO : 盘点单对象（返回的对象）
     * @param map : （产品规格 和产品的关联容器 ，键是产品规格 ，值是规格对象）
     * @param storage : （库存对象）
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO>
     */
    private ResponseResult<InventoryOrderDTO> assembleData(InventoryOrderDTO orderDTO,
                                                           Map<Integer,ConfigProductSpec> map,
                                                           List<StorageInventory>  storage){
        //库存判空
        if(CollectionUtils.isEmpty(storage)){
            log.info("通过规格id无法从库存从获得产品信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FUND_PRODUCT_BY_STORAGE);
        }
        //不为空,则遍历返回的仓库数据
        List<InventoryProductDetailDTO> data =storage.stream().map(obj->{
            //获得产品规格对象
            ConfigProductSpec spec=map.get(obj.getProductSpecId());
            String productName=spec==null?obj.getProductName():spec.getProductName();
            String specName=spec==null?obj.getProductSpecName():spec.getSpecName();
            return new InventoryProductDetailDTO(obj.getId(),obj.getProductId(),
                    productName,obj.getProductSpecId(),specName)
                    .setStorageId(obj.getStorageId())
                    .setStorageName(obj.getStorageName())
                    .setProductUnit(obj.getProductUnit())
                    .setAccountNum(BigDecimalUtils.isNull(obj.getProductNumber()));

        }).collect(Collectors.toList());
        //将该信息放入缓存，方便后面保存的时候查看
        orderDTO.setList(data).setImmutable(Status.FALSE.getKey());
        // TODO 暂不考虑缓存异常的问题 缓存两小时
        jedisService.setObject(orderDTO.getInventoryOrderId(),orderDTO,ConstantsUtil.TWO_HOURS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,orderDTO);
    }
    /**
     * @notes: 盘点类型为按仓库盘点，创建盘点单
     * @Author: junyang.li
     * @Date: 13:49 2019/11/26
     * @param userInfo : 当前操作人
     * @param categoryId : 仓库id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    private ResponseResult<InventoryOrderDTO> createInventoryByWarehouse(AuthPlatformUserInfo userInfo,
                                                                           List<Integer> categoryId){
        //判空，未勾选仓库
        if(CollectionUtils.isEmpty(categoryId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SELECTION_STORAGE);
        }
        Integer factoryId=userInfo.getFactoryId();
        //根据规格id查询对应的仓库及产品数量
        List<StorageInventory>  storage=storageInventoryService.getStorageByStorageIds(factoryId,categoryId);
        //判空
        if(CollectionUtils.isEmpty(storage)){
            log.info("盘点类型为按仓库盘点未查询到产品和规格信息，仓库id是:{}", JSON.toJSONString(categoryId));
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FUND_PRODUCT_BY_STORAGE);
        }
        //获得所有的规格信息
        List<Integer> productSpecIds=storage.stream()
                .map(StorageInventory::getProductSpecId).collect(Collectors.toList());
        ProductSpecParam param=new ProductSpecParam().setFactoryId(userInfo.getFactoryId())
                .setProductSpecIds(productSpecIds);
        //查询所有品类的产品
        List<ConfigProductSpec> list=configProductSpecService.getProductSpecByProductIds(param);
        //初始化产品规格对应的产品容器
        Map<Integer,ConfigProductSpec> map=new HashMap<>(64);
        list.forEach(obj->map.put(obj.getId(),obj));
        //创建订单id
        InventoryOrderDTO orderDTO=new InventoryOrderDTO()
                .setInventoryOrderId(this.createBusinessId())
                .setFactoryId(factoryId)
                .setEnterpriseId(userInfo.getOrgId())
                .setInventoryType(EnumInventoryType.WAREHOUSE.getCode());
        //使用公共方法组装数据
        return this.assembleData(orderDTO,map,storage);
    }

    /**
     * @notes: 为盘点单创建业务id
     * @Author: junyang.li
     * @Date: 14:40 2019/11/26
     * @return: java.lang.String
     */
    private String createBusinessId(){
        String key=ConstantsUtil.INVENTORY_ORDER+ DateUtils.format(DateTime.now().toDate(),DateUtils.YMD);
        try {
            //获得自增数值
            Integer autoNum = jedisService.incr(key,ConstantsUtil.OVERDUE);
            //格式化业务id
            return key+String.format("%03d",autoNum);
        }catch (Exception e){
            log.info("从 redis 中获取盘点单编号异常，异常信息是:{}",e);
            int autoNum = stockInventoryMapper.countCurDateByOrder();
            autoNum ++;
            jedisService.setIncr(key,autoNum);
            //从数据库中查询
           return key+String.format("%03d",autoNum);
        }
    }
    /**
     * @notes: 盘点类型为全部盘点
     * @Author: junyang.li
     * @Date: 10:05 2019/11/26
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO>>
     */
    private ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDescByAll(){
        //全部盘点不返回任何数据，默认全部盘点
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
    }
    /**
     * @notes: 通过产品类型进行盘点
     * @Author: junyang.li
     * @Date: 10:06 2019/11/26
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO>>
     */
    private ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDescByProductType(AuthPlatformUserInfo userInfo){
        //产品类型判断，返回产品类型列表查询
        List<ConfigProductCategoryDTO> list=productCategoryService.getProductCategoryList(userInfo);
        List<InventoryCategoryDTO> data=new ArrayList<>();
        if(!CollectionUtils.isEmpty(list)){
            list.forEach(obj->data.add(new InventoryCategoryDTO(obj.getId(),obj.getName())));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,data);
    }
    /**
     * @notes: 通过产品进行盘点
     * @Author: junyang.li
     * @Date: 10:06 2019/11/26
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO>>
     */
    private ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDescByProduct(AuthPlatformUserInfo userInfo){
        //查询产品
        List<ConfigProduct>  list=configProductService.getProductList(userInfo);
        List<InventoryCategoryDTO> data=new ArrayList<>();
        list.forEach(obj->data.add(new InventoryCategoryDTO(obj.getId(),obj.getName())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,data);
    }
    /**
     * @notes: 通过仓库进行盘点
     * @Author: junyang.li
     * @Date: 10:06 2019/11/26
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO>>
     */
    private ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDescByWarehouse(AuthPlatformUserInfo userInfo){
        List<ConfigRepository> list=configRepositoryService.getRepositoryList(userInfo);
        List<InventoryCategoryDTO> data=new ArrayList<>();
        list.forEach(obj->data.add(new InventoryCategoryDTO(obj.getId(),obj.getName())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,data);
    }

}
