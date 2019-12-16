package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ProductStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.StorageManagerListDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientDetail;
import com.bee.platform.cloud.si.manufacture.entity.StorageInventory;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @ClassName: StorageDetail
 * @Description: 仓库明细表mapper
 * @Author: fei.sun
 * @Date: 2019/9/23 13:49
 * @Version: 1.0
 */
public interface StorageInventoryMapper extends BaseMapper<StorageInventory> {

    int updateInventory(List<ProIngredientDetail> list);

    /**
     * 查询成品、主料、配料查询
     * @param typeName
     * @param productName
     * @return
     */
    List<StorageManagerListDTO> selectStorageManagerList(Pagination pagination,@Param("typeName") String typeName
            ,@Param("productName") String productName,@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询其他
     * @param pagination a
     * @param productName b
     * @return c
     */
    List<StorageManagerListDTO> selectOtherStorageManagerList(Pagination pagination, @Param("productName") String productName
    ,@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * @descriptin 查询库存中已存在的非成品产品列表
     * @author xin.huang
     * @param
     * @date 2019/10/10
     * @return
     */
    List<ProductStorageDTO> findStorageProducts(@Param("enterpriseId") Integer enterpriseId,
                                                @Param("factoryId") Integer factoryId);


    BigDecimal getOriginalStock(Map<String,Object> map);
}
