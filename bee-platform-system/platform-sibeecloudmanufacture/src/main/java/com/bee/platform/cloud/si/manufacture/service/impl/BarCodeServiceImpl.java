package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BarCodeMapper;
import com.bee.platform.cloud.si.manufacture.entity.BarCode;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.common.enums.Status;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 条形码表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-08
 */
@Slf4j
@Service
public class BarCodeServiceImpl extends ServiceImpl<BarCodeMapper, BarCode> implements BarCodeService {

    @Override
    public BarCode getByCode(String code) {
        return this.selectOne(new EntityWrapper<>(new BarCode().setCode(code).setStatus(Status.TRUE.getKey())));
    }

    @Override
    public boolean updateCodeUsed(BarCode barCode) {
        return this.updateById(barCode.setUsed(Status.TRUE.getKey()));
    }

    @Override
    public boolean updateCodeBatchUsed(List<String> codeList) {
        List<BarCode> barCodeList = this.selectList(new EntityWrapper<>(new BarCode().setStatus(Status.TRUE.getKey()))
                .in("code", codeList));
        if (CollectionUtils.isEmpty(barCodeList)) {
            return false;
        }
        barCodeList.forEach(a -> a.setUsed(Status.TRUE.getKey()));
        return this.updateBatchById(barCodeList);
    }

    @Override
    public BarCode checkCodeExist(String code) {
        return this.selectOne(new EntityWrapper<>(new BarCode()
                .setCode(code)
                .setStatus(Status.TRUE.getKey())));
    }
}
