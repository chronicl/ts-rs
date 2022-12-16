use poem::web::{Data, Form, Json, Path, Query};

use super::{impl_wrapper, Dependency, TS};

impl_wrapper!(impl<T: TS> TS for Data<T>);
impl_wrapper!(impl<T: TS> TS for Form<T>);
impl_wrapper!(impl<T: TS> TS for Json<T>);
impl_wrapper!(impl<T: TS> TS for Path<T>);
impl_wrapper!(impl<T: TS> TS for Query<T>);
