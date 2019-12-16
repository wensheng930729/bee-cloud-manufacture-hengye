import React from 'react';
import PropTypes from 'prop-types';
import { DatePicker } from 'antd';
import moment from 'moment';

const CustomDatePicker = props => {
  const { showTime, value, onChange, format, ...rest } = props;

  return (
    <DatePicker
      format={format}
      value={moment(value, format)}
      onChange={val => onChange(moment(val).format(format))}
      allowClear={false}
      showTime={showTime}
      {...rest}
    />
  );
};

CustomDatePicker.propTypes = {
  showTime: PropTypes.bool,
  value: PropTypes.string,
  onChange: PropTypes.func,
  format: PropTypes.string,
};

CustomDatePicker.defaultProps = {
  showTime: true,
  value: '',
  onChange: undefined,
  format: 'YYYY-MM-DD HH:mm:ss',
};

export default CustomDatePicker;
