package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.LocalDateUtils;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @ClassName: StorageTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/9/24 17:23
 * @Version: 1.0
 */
public class StorageTest {
    public static void main(String[] args) {
        BigDecimal a = new BigDecimal("5.00");
        BigDecimal b = new BigDecimal("-3.00");
        System.out.println(a.add(b));
//        test();
    }
    private static void test(){
        /*ConfigRepository c =null;
//        c.setName("111");
        String s = Optional.ofNullable(c).map(ConfigRepository::getName).orElse("");
        System.out.println(s);*/
        List<FinishedProductBeOutOfStorage> freeFinishedProductBeOutOfStorages = new ArrayList<>();
        FinishedProductBeOutOfStorage f1 = new FinishedProductBeOutOfStorage().setContractCarId("1");
        FinishedProductBeOutOfStorage f2 = new FinishedProductBeOutOfStorage().setContractCarId("2");
        FinishedProductBeOutOfStorage f3 = new FinishedProductBeOutOfStorage().setContractCarId("3");
        FinishedProductBeOutOfStorage f4 = new FinishedProductBeOutOfStorage().setContractCarId("1");
        FinishedProductBeOutOfStorage f5 = new FinishedProductBeOutOfStorage().setContractCarId("2");
        freeFinishedProductBeOutOfStorages.add(f1);
        freeFinishedProductBeOutOfStorages.add(f2);
        freeFinishedProductBeOutOfStorages.add(f3);
        freeFinishedProductBeOutOfStorages.add(f4);
        freeFinishedProductBeOutOfStorages.add(f5);
        List<FinishedProductBeOutOfStorage> result =
                freeFinishedProductBeOutOfStorages.stream().collect(Collectors.collectingAndThen(
                        Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(FinishedProductBeOutOfStorage::getContractCarId)))
                        ,ArrayList::new));
        result.forEach(System.out::println);
    }
}
